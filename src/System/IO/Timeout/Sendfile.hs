import           Network.Socket (Socket, ShutdownCmd(..), shutdown)
import           Network.Socket.SendFile (Iter(..), ByteCount, Offset, unsafeSendFileIterWith')
import           Network.Socket.ByteString (sendAll)
import qualified Network.Socket.ByteString.Lazy as LS (getContents)

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
