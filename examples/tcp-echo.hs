module Main where

import Control.Concurrent -- (forkIO)
import Control.Exception
import Control.Monad (forever)
import Network (PortID(..), listenOn, sClose)
import Network.Socket (Socket, accept)
import System.IO.Timeout
import qualified System.IO.Timeout.Socket as N

main :: IO ()
main =
    do tm <- initialize 
       s' <- listenOn (PortNumber 8000)
       forever $ 
               do (s, _) <- accept s'
                  forkIO $ (do thandle <- register tm 30
                               echo thandle s
                               cancel thandle
                               return ()) `finally` (sClose s)

echo :: TimeoutHandle -> Socket -> IO ()
echo thandle sock =
    do c <- N.getContents thandle sock
       N.sendAll thandle sock c
