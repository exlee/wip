-- | Runtime loading of C libraries (dlopen/dlsym) and the call wrappers for
-- each C signature the app uses. Counterpart of ctypes-foreign's @Dl@ and
-- @Foreign.foreign ~from@.
module Ffi
  ( DL
  , fromBool
  , toBool
  , module Ffi
  ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString as B
import Data.Int (Int32)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word64, Word8)
import Foreign.C.String (CString)
import Foreign.C.Types
import Foreign.Marshal.Utils (fromBool, toBool)
import Foreign.Ptr (FunPtr, Ptr)
import System.Posix.DynamicLinker (DL, RTLDFlags, dlopen, dlsym)

-- | The first library in @names@ that loads.
openFirst :: [RTLDFlags] -> [FilePath] -> IO (Maybe DL)
openFirst _ [] = pure Nothing
openFirst flags (name : rest) =
  try (dlopen name flags) >>= \case
    Right dl -> pure (Just dl)
    Left (_ :: IOException) -> openFirst flags rest

-- | Looks up a function; throws when it is missing.
symbol :: DL -> String -> IO (FunPtr a)
symbol = dlsym

-- | Passes a Haskell string to C as NUL-terminated UTF-8.
withUtf8 :: String -> (CString -> IO a) -> IO a
withUtf8 s = B.useAsCString (TE.encodeUtf8 (T.pack s))

peekUtf8 :: CString -> IO String
peekUtf8 p = T.unpack . TE.decodeUtf8Lenient <$> B.packCString p

-- Call wrappers, named after their C signature: v = void, b = bool, p = pointer,
-- s = string, i = int, f = float, u8/u32/u64 = unsigned, z = size_t, l = long.
-- Unsafe calls hold the runtime like ctypes' default; only blocking calls are safe.

foreign import ccall unsafe "dynamic" call_v_s :: FunPtr (IO CString) -> IO CString
foreign import ccall unsafe "dynamic" call_v_v :: FunPtr (IO ()) -> IO ()
foreign import ccall unsafe "dynamic" call_v_b :: FunPtr (IO CBool) -> IO CBool
foreign import ccall unsafe "dynamic" call_v_u32 :: FunPtr (IO Word32) -> IO Word32
foreign import ccall unsafe "dynamic" call_s_b :: FunPtr (CString -> IO CBool) -> CString -> IO CBool
foreign import ccall unsafe "dynamic" call_s_p :: FunPtr (CString -> IO (Ptr ())) -> CString -> IO (Ptr ())
foreign import ccall unsafe "dynamic" call_ss_b :: FunPtr (CString -> CString -> IO CBool) -> CString -> CString -> IO CBool
foreign import ccall unsafe "dynamic" call_sf_p :: FunPtr (CString -> CFloat -> IO (Ptr ())) -> CString -> CFloat -> IO (Ptr ())
foreign import ccall unsafe "dynamic" call_u32_b :: FunPtr (Word32 -> IO CBool) -> Word32 -> IO CBool
foreign import ccall unsafe "dynamic" call_u32p_b :: FunPtr (Word32 -> Ptr CInt -> IO CBool) -> Word32 -> Ptr CInt -> IO CBool
foreign import ccall unsafe "dynamic" call_u32sp_p :: FunPtr (Word32 -> CString -> Ptr () -> IO (Ptr ())) -> Word32 -> CString -> Ptr () -> IO (Ptr ())
foreign import ccall unsafe "dynamic" call_siiu64_p :: FunPtr (CString -> CInt -> CInt -> Word64 -> IO (Ptr ())) -> CString -> CInt -> CInt -> Word64 -> IO (Ptr ())
foreign import ccall unsafe "dynamic" call_i_p :: FunPtr (CInt -> IO (Ptr ())) -> CInt -> IO (Ptr ())
foreign import ccall unsafe "dynamic" call_p_v :: FunPtr (Ptr () -> IO ()) -> Ptr () -> IO ()
foreign import ccall unsafe "dynamic" call_p_b :: FunPtr (Ptr () -> IO CBool) -> Ptr () -> IO CBool
foreign import ccall unsafe "dynamic" call_p_i :: FunPtr (Ptr () -> IO CInt) -> Ptr () -> IO CInt
foreign import ccall unsafe "dynamic" call_p_f :: FunPtr (Ptr () -> IO CFloat) -> Ptr () -> IO CFloat
foreign import ccall unsafe "dynamic" call_p_p :: FunPtr (Ptr () -> IO (Ptr ())) -> Ptr () -> IO (Ptr ())
foreign import ccall unsafe "dynamic" call_p_u32 :: FunPtr (Ptr () -> IO Word32) -> Ptr () -> IO Word32
foreign import ccall unsafe "dynamic" call_pi_v :: FunPtr (Ptr () -> CInt -> IO ()) -> Ptr () -> CInt -> IO ()
foreign import ccall unsafe "dynamic" call_pii_b :: FunPtr (Ptr () -> CInt -> CInt -> IO CBool) -> Ptr () -> CInt -> CInt -> IO CBool
foreign import ccall unsafe "dynamic" call_pf_b :: FunPtr (Ptr () -> CFloat -> IO CBool) -> Ptr () -> CFloat -> IO CBool
foreign import ccall unsafe "dynamic" call_pff_b :: FunPtr (Ptr () -> CFloat -> CFloat -> IO CBool) -> Ptr () -> CFloat -> CFloat -> IO CBool
foreign import ccall unsafe "dynamic" call_pffs_b :: FunPtr (Ptr () -> CFloat -> CFloat -> CString -> IO CBool) -> Ptr () -> CFloat -> CFloat -> CString -> IO CBool
foreign import ccall unsafe "dynamic" call_ppi_b :: FunPtr (Ptr () -> Ptr CInt -> IO CBool) -> Ptr () -> Ptr CInt -> IO CBool
foreign import ccall unsafe "dynamic" call_ppf_b :: FunPtr (Ptr () -> Ptr CFloat -> IO CBool) -> Ptr () -> Ptr CFloat -> IO CBool
foreign import ccall unsafe "dynamic" call_ppp_b :: FunPtr (Ptr () -> Ptr CInt -> Ptr CInt -> IO CBool) -> Ptr () -> Ptr CInt -> Ptr CInt -> IO CBool
foreign import ccall unsafe "dynamic" call_pps_p :: FunPtr (Ptr () -> Ptr () -> CString -> CSize -> IO (Ptr ())) -> Ptr () -> Ptr () -> CString -> CSize -> IO (Ptr ())
foreign import ccall unsafe "dynamic" call_ps_p :: FunPtr (Ptr () -> CString -> IO (Ptr ())) -> Ptr () -> CString -> IO (Ptr ())
foreign import ccall unsafe "dynamic" call_pu32_p :: FunPtr (Ptr () -> Word32 -> IO (Ptr ())) -> Ptr () -> Word32 -> IO (Ptr ())
foreign import ccall unsafe "dynamic" call_pu8x4_b :: FunPtr (Ptr () -> Word8 -> Word8 -> Word8 -> Word8 -> IO CBool) -> Ptr () -> Word8 -> Word8 -> Word8 -> Word8 -> IO CBool
foreign import ccall unsafe "dynamic" call_pszpp_b :: FunPtr (Ptr () -> CString -> CSize -> Ptr CInt -> Ptr CInt -> IO CBool) -> Ptr () -> CString -> CSize -> Ptr CInt -> Ptr CInt -> IO CBool
foreign import ccall unsafe "dynamic" call_pszipp_b :: FunPtr (Ptr () -> CString -> CSize -> CInt -> Ptr CInt -> Ptr CInt -> IO CBool) -> Ptr () -> CString -> CSize -> CInt -> Ptr CInt -> Ptr CInt -> IO CBool
foreign import ccall unsafe "dynamic" call_pszu32i_p :: FunPtr (Ptr () -> CString -> CSize -> Word32 -> CInt -> IO (Ptr ())) -> Ptr () -> CString -> CSize -> Word32 -> CInt -> IO (Ptr ())
foreign import ccall unsafe "dynamic" call_pfpf_u32 :: FunPtr (Ptr CFloat -> Ptr CFloat -> IO Word32) -> Ptr CFloat -> Ptr CFloat -> IO Word32
foreign import ccall unsafe "dynamic" call_ppl_v :: FunPtr (Ptr () -> Ptr () -> CLong -> IO ()) -> Ptr () -> Ptr () -> CLong -> IO ()
foreign import ccall unsafe "dynamic" call_ppul_v :: FunPtr (Ptr () -> Ptr () -> CULong -> IO ()) -> Ptr () -> Ptr () -> CULong -> IO ()
foreign import ccall unsafe "dynamic" call_ppb_v :: FunPtr (Ptr () -> Ptr () -> CBool -> IO ()) -> Ptr () -> Ptr () -> CBool -> IO ()
-- Blocks while waiting for events, so other Haskell threads keep running.
foreign import ccall safe "dynamic" call_pu8i32_b :: FunPtr (Ptr Word8 -> Int32 -> IO CBool) -> Ptr Word8 -> Int32 -> IO CBool
