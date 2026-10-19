-- | Loopback TCP sockets through libc, wrapped in 'Handle's. Stands in for
-- OCaml's Unix module; the @network@ package is not a GHC boot package.
module Wip.Socket
  ( Socket
  , listenLoopback
  , accept
  , connectLoopback
  , closeSocket
  , socketHandle
  ) where

import Control.Exception (bracketOnError)
import Foreign
import Foreign.C
import System.IO
import System.Posix.IO (fdToHandle)
import System.Posix.Types (Fd (..))

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

newtype Socket = Socket CInt

foreign import ccall unsafe "socket" c_socket :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "setsockopt" c_setsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> CUInt -> IO CInt
foreign import ccall unsafe "bind" c_bind :: CInt -> Ptr () -> CUInt -> IO CInt
foreign import ccall unsafe "listen" c_listen :: CInt -> CInt -> IO CInt
foreign import ccall unsafe "getsockname" c_getsockname :: CInt -> Ptr () -> Ptr CUInt -> IO CInt
foreign import ccall safe "accept" c_accept :: CInt -> Ptr () -> Ptr CUInt -> IO CInt
foreign import ccall safe "connect" c_connect :: CInt -> Ptr () -> CUInt -> IO CInt
foreign import ccall unsafe "close" c_close :: CInt -> IO CInt

addrSize :: Int
addrSize = #{size struct sockaddr_in}

-- | Fills a sockaddr_in for 127.0.0.1:port. Port and address are written
-- byte by byte in network order.
withLoopback :: Int -> (Ptr () -> IO a) -> IO a
withLoopback port f = allocaBytes addrSize $ \p -> do
  fillBytes p 0 addrSize
#ifdef __APPLE__
  #{poke struct sockaddr_in, sin_len} p (fromIntegral addrSize :: Word8)
#endif
  #{poke struct sockaddr_in, sin_family} p (#{const AF_INET} :: #{type sa_family_t})
  let portOff = #{offset struct sockaddr_in, sin_port}
      addrOff = #{offset struct sockaddr_in, sin_addr}
  pokeByteOff p portOff (fromIntegral (port `shiftR` 8) :: Word8)
  pokeByteOff p (portOff + 1) (fromIntegral port :: Word8)
  mapM_ (\(i, b) -> pokeByteOff p (addrOff + i) (b :: Word8)) (zip [0 ..] [127, 0, 0, 1])
  f p

newSocket :: IO CInt
newSocket = throwErrnoIfMinus1 "socket" (c_socket #{const AF_INET} #{const SOCK_STREAM} 0)

-- | Binds 127.0.0.1:port (0 picks a free port) and listens; returns the
-- socket and the bound port.
listenLoopback :: Int -> Int -> IO (Socket, Int)
listenLoopback port backlog =
  bracketOnError newSocket c_close $ \fd -> do
    with (1 :: CInt) $ \one ->
      throwErrnoIfMinus1_ "setsockopt" $
        c_setsockopt fd #{const SOL_SOCKET} #{const SO_REUSEADDR} one (fromIntegral (sizeOf (0 :: CInt)))
    withLoopback port $ \addr ->
      throwErrnoIfMinus1_ "bind" (c_bind fd addr (fromIntegral addrSize))
    throwErrnoIfMinus1_ "listen" (c_listen fd (fromIntegral backlog))
    bound <- allocaBytes addrSize $ \addr -> with (fromIntegral addrSize) $ \len -> do
      throwErrnoIfMinus1_ "getsockname" (c_getsockname fd addr len)
      let portOff = #{offset struct sockaddr_in, sin_port}
      hi <- peekByteOff addr portOff :: IO Word8
      lo <- peekByteOff addr (portOff + 1) :: IO Word8
      pure (fromIntegral hi * 256 + fromIntegral lo)
    pure (Socket fd, bound)

-- | Waits for the next connection; blocks only the calling thread.
accept :: Socket -> IO CInt
accept (Socket fd) = throwErrnoIfMinus1Retry "accept" (c_accept fd nullPtr nullPtr)

-- | Connects to 127.0.0.1:port. Failures are 'IOError's carrying the errno
-- (ECONNREFUSED when nothing listens).
connectLoopback :: Int -> IO CInt
connectLoopback port =
  bracketOnError newSocket c_close $ \fd -> do
    withLoopback port $ \addr ->
      throwErrnoIfMinus1Retry_ "connect" (c_connect fd addr (fromIntegral addrSize))
    pure fd

closeSocket :: Socket -> IO ()
closeSocket (Socket fd) = () <$ c_close fd

-- | A binary, read-write 'Handle' owning the connected descriptor.
socketHandle :: CInt -> IO Handle
socketHandle fd = do
  h <- fdToHandle (Fd fd)
  hSetBinaryMode h True
  hSetBuffering h (BlockBuffering Nothing)
  pure h
