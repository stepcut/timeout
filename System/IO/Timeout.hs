{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{- | 
-- borrowed from snap-server. Check there periodically for updates.
-}
module System.IO.Timeout where

import           Control.Concurrent            (ThreadId, forkIO, killThread, threadDelay, threadWaitWrite)
import           Control.Exception             (catch, SomeException)
import           Control.Monad                 (liftM)
import qualified Data.ByteString.Char8         as B
import qualified Data.ByteString.Lazy.Char8    as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString               as S
import qualified Network.Socket.ByteString as N
import           Data.DList (DList)
import qualified Data.DList                    as D
import           Data.Word
import           Data.IORef
import           Data.List (foldl')
import qualified Data.PSQueue as PSQ
import           Data.PSQueue (PSQ)
import           Data.Time.Clock.POSIX(POSIXTime, getPOSIXTime)
import qualified System.IO.TimeoutTable as TT
import           System.IO.TimeoutTable (TimeoutTable)
import           Network.Socket (Socket, ShutdownCmd(..), shutdown)
import           Network.Socket.SendFile (Iter(..), ByteCount, Offset, unsafeSendFileIterWith')
import           Network.Socket.ByteString (sendAll)
import qualified Network.Socket.ByteString.Lazy as LS (getContents)
import           Prelude hiding (catch)
import           System.IO (Handle, hClose, hIsEOF, hWaitForInput)
import           System.IO.Unsafe (unsafeInterleaveIO)

debug = const (return ())

data TimeoutHandle = TimeoutHandle
    { _threadHash              :: !Word
    , _threadId                :: !ThreadId
    , _timeoutTable            :: TimeoutTable
    , _getApproximatePOSIXTime :: IO POSIXTime
    }

timeoutThread :: TimeoutTable -> IO POSIXTime -> IO ThreadId
timeoutThread timeoutTable getApproximatePOSIXTime = do
    forkIO $ loop `catch` (\(_::SomeException) -> killAll)

  where
    table = timeoutTable
    loop = do
        debug "timeoutThread: waiting for activity on thread table"
        TT.waitForActivity table
        debug "timeoutThread: woke up, killing old connections"
        killTooOld
        loop


    killTooOld = do
        now    <- getApproximatePOSIXTime
        TT.killOlderThan (now - tIMEOUT) table

    -- timeout = 30 seconds
    tIMEOUT = 30

    killAll = do
        debug "timeoutThread: shutdown, killing all connections"
        TT.killAll table


tickleTimeout :: TimeoutHandle -> IO ()
tickleTimeout thandle = do
    now <- _getApproximatePOSIXTime thandle
    TT.insert thash tid now tt
    return ()
        where
          thash = _threadHash   thandle
          tid   = _threadId     thandle
          tt    = _timeoutTable thandle
{-# INLINE tickleTimeout #-}

cancelTimeout :: TimeoutHandle -> IO ()
cancelTimeout thandle =
    TT.delete thash tid tt
        where
          thash = _threadHash   thandle
          tid   = _threadId     thandle
          tt    = _timeoutTable thandle
{-# INLINE cancelTimeout #-}


unsafeSendFileTickle :: TimeoutHandle -> Handle -> FilePath -> Offset -> ByteCount -> IO ()
unsafeSendFileTickle thandle outp fp offset count =
    unsafeSendFileIterWith' (iterTickle thandle) outp fp 65536 offset count

iterTickle :: TimeoutHandle -> IO Iter -> IO ()
iterTickle thandle = 
    iterTickle' 
    where
      iterTickle' :: (IO Iter -> IO ())
      iterTickle' iter =
          do r <- iter
             tickleTimeout thandle
             case r of
               (Done _) ->
                      return ()
               (WouldBlock _ fd cont) ->
                   do threadWaitWrite fd
                      iterTickle' cont
               (Sent _ cont) ->
                   do iterTickle' cont
