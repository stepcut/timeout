module System.IO.Timeout.Socket where

import Control.Monad (liftM)
import qualified Data.ByteString.Lazy.Char8    as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString               as S
import           Network.Socket (Socket, ShutdownCmd(..), shutdown)
import qualified Network.Socket.ByteString as N
import System.IO.Timeout
import           System.IO.Unsafe (unsafeInterleaveIO)

sendAll :: TimeoutHandle -> Socket -> L.ByteString -> IO ()
sendAll thandle sock cs =
    do L.foldrChunks (\c rest -> N.sendAll sock c >> tickle thandle 30 >> rest) (return ()) cs
{-# INLINE sendAll #-}

getContents :: TimeoutHandle 
             -> Socket           -- ^ Connected socket
             -> IO L.ByteString  -- ^ Data received
getContents thandle sock = loop where
  loop = unsafeInterleaveIO $ do
    s <- N.recv sock 65536
    tickle thandle 30
    if S.null s
      then shutdown sock ShutdownReceive >> return L.Empty
      else L.Chunk s `liftM` loop

