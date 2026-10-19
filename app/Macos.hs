-- | macOS window tweaks through the Objective-C runtime.
module Macos (floatOverEverything, statusLevel, floatingLevel) where

import Ffi
import Foreign
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.DynamicLinker (RTLDFlags (..), dlopen)

objc :: DL
objc = unsafePerformIO (dlopen "/usr/lib/libobjc.A.dylib" [RTLD_NOW])
{-# NOINLINE objc #-}

msgSend :: IO (FunPtr a)
msgSend = symbol objc "objc_msgSend"

sel :: String -> IO (Ptr ())
sel name = do
  register <- call_s_p <$> symbol objc "sel_registerName"
  withUtf8 name register

-- NSWindowLevel values from CGWindowLevel.h.
floatingLevel, statusLevel :: Int
floatingLevel = 3
statusLevel = 25

-- NSWindowCollectionBehavior flags.
canJoinAllSpaces, stationary, ignoresCycle, fullScreenAuxiliary :: Int
canJoinAllSpaces = 1
stationary = 16
ignoresCycle = 64
fullScreenAuxiliary = 256

-- | Keeps the window above all apps, on every Space and over full-screen apps,
-- and visible while the app is inactive (it never activates).
-- objc_msgSend is called through a pointer typed for each message.
floatOverEverything :: Int -> Ptr () -> IO ()
floatOverEverything level nswindow = do
  setLong <- call_ppl_v <$> msgSend
  setULong <- call_ppul_v <$> msgSend
  setBool <- call_ppb_v <$> msgSend
  sel "setLevel:" >>= \s -> setLong nswindow s (fromIntegral level)
  sel "setCollectionBehavior:" >>= \s ->
    setULong nswindow s (fromIntegral (canJoinAllSpaces .|. stationary .|. ignoresCycle .|. fullScreenAuxiliary))
  sel "setHidesOnDeactivate:" >>= \s -> setBool nswindow s (fromBool False)
