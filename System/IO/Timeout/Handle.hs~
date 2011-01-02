
{-
hGetContentsN :: Int -> TimeoutHandle -> Handle -> IO L.ByteString
hGetContentsN k thandle h = lazyRead -- TODO close on exceptions
  where
    lazyRead = unsafeInterleaveIO loop

    loop = do
        c <- S.hGetNonBlocking h k
        if S.null c
          then do eof <- hIsEOF h
                  if eof then hClose h >> cancelTimeout thandle >> return L.Empty
                         else hWaitForInput h (-1)
                            >> loop

          --then hClose h >> return Empty
          else do tickleTimeout thandle
                  cs <- lazyRead
                  return (L.Chunk c cs)

hGetContents' :: TimeoutHandle -> Handle -> IO L.ByteString
hGetContents' thandle h = hGetContentsN L.defaultChunkSize thandle h
-}
