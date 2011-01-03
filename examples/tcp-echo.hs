module Main where

import Control.Concurrent -- (forkIO)
import Control.Exception
import Control.Monad (forever)
import Data.ByteString.Lazy.Char8 as L
import Data.Concurrent.HashMap (hashString)
import Network (PortID(..), listenOn, sClose)
import Network.Socket (Socket, accept)
import System.IO.Timeout
import System.IO.TimeoutTable as TT
import qualified System.IO.Timeout.Socket as N
import Data.Time.Clock.POSIX

main :: IO ()
main =
    do tt <- TT.new
       ttid <- timeoutThread tt getPOSIXTime 
       s' <- listenOn (PortNumber 8000)
       forever $ 
               do (s, _) <- accept s'
                  forkIO $ (do tid <- myThreadId
                               let thandle = TimeoutHandle (hashString (show tid)) tid tt getPOSIXTime
                               tickleTimeout thandle
                               echo thandle s
                               cancelTimeout thandle
                               return ()) `finally` (sClose s)

echo :: TimeoutHandle -> Socket -> IO ()
echo thandle sock =
    do c <- N.getContents thandle sock
       N.sendAll thandle sock c
