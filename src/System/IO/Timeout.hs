{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{- | 
-- borrowed from snap-server. Check there periodically for updates.
-}
module System.IO.Timeout 
    ( TimeoutManager
    , TimeoutHandle
    , initialize'
    , initialize
    , destroy
    , register
    , tickle
    , cancel
    , withTimeout
    ) where

import           Control.Concurrent      (ThreadId, forkIO, killThread, myThreadId)
import           Control.Exception       (AsyncException(ThreadKilled), SomeException, catch, throw)
import           Control.Monad           (liftM)
import           Data.Concurrent.HashMap (hashString)
import           Data.Word
import           Data.Time.Clock.POSIX   (POSIXTime, getPOSIXTime)
import qualified System.IO.TimeoutTable  as TT
import           System.IO.TimeoutTable  (TimeoutTable)
import           Prelude                 hiding (catch)

debug :: String -> IO ()
debug = const (return ())

data TimeoutHandle = TimeoutHandle
    { _threadHash              :: !Word
    , _threadId                :: !ThreadId
    , _timeoutTable            :: TimeoutTable
    , _getApproximatePOSIXTime :: IO POSIXTime
    }

data TimeoutManager = TimeoutManager 
    { _tm_getApproximatePOSIXTime :: IO POSIXTime
    , _tm_timeoutThread           :: ThreadId
    , _tm_timeoutTable            :: TimeoutTable
    }

initialize' :: IO POSIXTime -> IO TimeoutManager
initialize' getPOSIXTime' =
    do tt <- TT.new
       tid <- timeoutThread tt getPOSIXTime'
       return $ TimeoutManager  { _tm_getApproximatePOSIXTime = getPOSIXTime'
                                , _tm_timeoutThread = tid
                                , _tm_timeoutTable = tt
                                }

initialize :: IO TimeoutManager
initialize = initialize' getPOSIXTime

destroy :: TimeoutManager -> IO ()
destroy tm =
    do killThread (_tm_timeoutThread tm)

register :: TimeoutManager -> Int -> IO TimeoutHandle
register tm timeout =
    do tid <- myThreadId 
       let th = TimeoutHandle { _threadHash              = hashString (show tid)
                              , _threadId                = tid
                              , _timeoutTable            = _tm_timeoutTable tm
                              , _getApproximatePOSIXTime = _tm_getApproximatePOSIXTime tm
                              }
       tickle th timeout
       return $ th

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


tickle :: TimeoutHandle -> Int -> IO ()
tickle thandle timeout = do
    now <- _getApproximatePOSIXTime thandle
    TT.insert thash tid now tt
    return ()
        where
          thash = _threadHash   thandle
          tid   = _threadId     thandle
          tt    = _timeoutTable thandle
{-# INLINE tickle #-}

cancel :: TimeoutHandle -> IO ()
cancel thandle =
    TT.delete thash tid tt
        where
          thash = _threadHash   thandle
          tid   = _threadId     thandle
          tt    = _timeoutTable thandle
{-# INLINE cancel #-}

withTimeout :: TimeoutManager -> Int -> (TimeoutHandle -> IO a) -> IO (Maybe a)
withTimeout tm timeout action =
    do th <- register tm timeout
       a <- (liftM Just (action th)) `catch` isThreadKilled
       cancel th
       return a
    where
      isThreadKilled :: AsyncException -> IO (Maybe a)
      isThreadKilled ThreadKilled = return Nothing
      isThreadKilled e            = throw e
