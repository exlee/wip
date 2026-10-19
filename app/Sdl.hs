{-# LANGUAGE MultiWayIf #-}

-- | Minimal SDL3 bindings, loaded with dlopen at first use. Only what the app needs.
module Sdl
  ( Window
  , Renderer
  , Cursor
  , Event (..)
  , lib
  , symbol
  , windowBorderless
  , windowHighPixelDensity
  , windowAlwaysOnTop
  , windowUtility
  , windowNotFocusable
  , cursorDefault
  , cursorMove
  , cursorPointer
  , getError
  , setHint
  , initVideo
  , quit
  , createWindow
  , destroyWindow
  , setWindowSize
  , setWindowPosition
  , getWindowPosition
  , getWindowPixelDensity
  , primaryUsableBounds
  , createRenderer
  , destroyRenderer
  , setColor
  , renderClear
  , renderPresent
  , fillRect
  , renderDebugText
  , setRenderScale
  , waitEvent
  , pollEvents
  , globalMouse
  , createSystemCursor
  , setCursor
  , openUrl
  , cocoaWindow
  , setClip
  ) where

import Control.Monad (void)
import Ffi hiding (symbol)
import qualified Ffi
import Foreign hiding (void)
import Foreign.C
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.DynamicLinker (RTLDFlags (..))
import Wip.Color (Color)
import Wip.Layout (roundf)

type Window = Ptr ()
type Renderer = Ptr ()
type Cursor = Ptr ()

lib :: DL
lib = unsafePerformIO $ do
  found <-
    openFirst
      [RTLD_NOW, RTLD_GLOBAL]
      [ "libSDL3.0.dylib"
      , "/opt/homebrew/lib/libSDL3.0.dylib"
      , "/usr/local/lib/libSDL3.0.dylib"
      , "libSDL3.so.0"
      , "libSDL3.so"
      ]
  maybe (fail "SDL3 shared library not found") pure found
{-# NOINLINE lib #-}

-- | Looks up an SDL function in the loaded library.
symbol :: String -> IO (FunPtr a)
symbol = Ffi.symbol lib

-- Constants
initVideoFlag :: Word32
initVideoFlag = 0x20

windowBorderless, windowHighPixelDensity, windowAlwaysOnTop, windowUtility, windowNotFocusable :: Word64
windowBorderless = 0x10
windowHighPixelDensity = 0x2000
windowAlwaysOnTop = 0x10000
windowUtility = 0x20000
windowNotFocusable = 0x80000000

eventQuit, eventWindowMouseLeave, eventWindowCloseRequested, eventMouseMotion, eventMouseButtonDown, eventMouseButtonUp :: Word32
eventQuit = 0x100
eventWindowMouseLeave = 0x20D
eventWindowCloseRequested = 0x210
eventMouseMotion = 0x400
eventMouseButtonDown = 0x401
eventMouseButtonUp = 0x402

cursorDefault, cursorMove, cursorPointer :: Int
cursorDefault = 0
cursorMove = 9
cursorPointer = 11

-- Functions, resolved once.
data Api = Api
  { sdlGetError :: IO CString
  , sdlSetHint :: CString -> CString -> IO CBool
  , sdlInit :: Word32 -> IO CBool
  , sdlQuit :: IO ()
  , sdlCreateWindow :: CString -> CInt -> CInt -> Word64 -> IO Window
  , sdlDestroyWindow :: Window -> IO ()
  , sdlSetWindowSize :: Window -> CInt -> CInt -> IO CBool
  , sdlSetWindowPosition :: Window -> CInt -> CInt -> IO CBool
  , sdlGetWindowPosition :: Window -> Ptr CInt -> Ptr CInt -> IO CBool
  , sdlGetWindowPixelDensity :: Window -> IO CFloat
  , sdlGetPrimaryDisplay :: IO Word32
  , sdlGetDisplayUsableBounds :: Word32 -> Ptr CInt -> IO CBool
  , sdlCreateRenderer :: Window -> CString -> IO Renderer
  , sdlDestroyRenderer :: Renderer -> IO ()
  , sdlSetRenderDrawColor :: Renderer -> Word8 -> Word8 -> Word8 -> Word8 -> IO CBool
  , sdlRenderClear :: Renderer -> IO CBool
  , sdlRenderFillRect :: Renderer -> Ptr CFloat -> IO CBool
  , sdlRenderDebugText :: Renderer -> CFloat -> CFloat -> CString -> IO CBool
  , sdlSetRenderScale :: Renderer -> CFloat -> CFloat -> IO CBool
  , sdlRenderPresent :: Renderer -> IO CBool
  , sdlWaitEventTimeout :: Ptr Word8 -> Int32 -> IO CBool
  , sdlGetGlobalMouseState :: Ptr CFloat -> Ptr CFloat -> IO Word32
  , sdlCreateSystemCursor :: CInt -> IO Cursor
  , sdlSetCursor :: Cursor -> IO CBool
  , sdlOpenURL :: CString -> IO CBool
  , sdlGetWindowProperties :: Window -> IO Word32
  , sdlGetPointerProperty :: Word32 -> CString -> Ptr () -> IO (Ptr ())
  , sdlSetRenderClipRect :: Renderer -> Ptr CInt -> IO CBool
  }

api :: Api
api = unsafePerformIO $ do
  let fn mk name = mk <$> symbol name
  Api
    <$> fn call_v_s "SDL_GetError"
    <*> fn call_ss_b "SDL_SetHint"
    <*> fn call_u32_b "SDL_Init"
    <*> fn call_v_v "SDL_Quit"
    <*> fn call_siiu64_p "SDL_CreateWindow"
    <*> fn call_p_v "SDL_DestroyWindow"
    <*> fn call_pii_b "SDL_SetWindowSize"
    <*> fn call_pii_b "SDL_SetWindowPosition"
    <*> fn call_ppp_b "SDL_GetWindowPosition"
    <*> fn call_p_f "SDL_GetWindowPixelDensity"
    <*> fn call_v_u32 "SDL_GetPrimaryDisplay"
    <*> fn call_u32p_b "SDL_GetDisplayUsableBounds"
    <*> fn call_ps_p "SDL_CreateRenderer"
    <*> fn call_p_v "SDL_DestroyRenderer"
    <*> fn call_pu8x4_b "SDL_SetRenderDrawColor"
    <*> fn call_p_b "SDL_RenderClear"
    <*> fn call_ppf_b "SDL_RenderFillRect"
    <*> fn call_pffs_b "SDL_RenderDebugText"
    <*> fn call_pff_b "SDL_SetRenderScale"
    <*> fn call_p_b "SDL_RenderPresent"
    <*> fn call_pu8i32_b "SDL_WaitEventTimeout"
    <*> fn call_pfpf_u32 "SDL_GetGlobalMouseState"
    <*> fn call_i_p "SDL_CreateSystemCursor"
    <*> fn call_p_b "SDL_SetCursor"
    <*> fn call_s_b "SDL_OpenURL"
    <*> fn call_p_u32 "SDL_GetWindowProperties"
    <*> fn call_u32sp_p "SDL_GetPointerProperty"
    <*> fn call_ppi_b "SDL_SetRenderClipRect"
{-# NOINLINE api #-}

-- Wrappers
getError :: IO String
getError = sdlGetError api >>= peekUtf8

check :: String -> CBool -> IO ()
check what ok = if toBool ok then pure () else getError >>= \e -> fail (what ++ ": " ++ e)

nonNull :: String -> Ptr a -> IO (Ptr a)
nonNull what p = if p == nullPtr then getError >>= \e -> fail (what ++ ": " ++ e) else pure p

setHint :: String -> String -> IO Bool
setHint k v = withUtf8 k $ \ck -> withUtf8 v $ \cv -> toBool <$> sdlSetHint api ck cv

initVideo :: IO ()
initVideo = sdlInit api initVideoFlag >>= check "SDL_Init"

quit :: IO ()
quit = sdlQuit api

createWindow :: String -> Int -> Int -> [Word64] -> IO Window
createWindow title w h flags =
  withUtf8 title $ \ct ->
    sdlCreateWindow api ct (fromIntegral w) (fromIntegral h) (foldl (.|.) 0 flags) >>= nonNull "SDL_CreateWindow"

destroyWindow :: Window -> IO ()
destroyWindow = sdlDestroyWindow api

setWindowSize, setWindowPosition :: Window -> Int -> Int -> IO Bool
setWindowSize w x y = toBool <$> sdlSetWindowSize api w (fromIntegral x) (fromIntegral y)
setWindowPosition w x y = toBool <$> sdlSetWindowPosition api w (fromIntegral x) (fromIntegral y)

getWindowPosition :: Window -> IO (Int, Int)
getWindowPosition w = alloca $ \px -> alloca $ \py -> do
  poke px 0
  poke py 0
  void (sdlGetWindowPosition api w px py)
  (,) <$> (fromIntegral <$> peek px) <*> (fromIntegral <$> peek py)

getWindowPixelDensity :: Window -> IO Double
getWindowPixelDensity w = realToFrac <$> sdlGetWindowPixelDensity api w

-- | (x, y, w, h) of the primary display's usable area.
primaryUsableBounds :: IO (Maybe (Int, Int, Int, Int))
primaryUsableBounds = allocaArray 4 $ \r -> do
  display <- sdlGetPrimaryDisplay api
  ok <- sdlGetDisplayUsableBounds api display r
  if toBool ok
    then peekArray 4 r >>= \case
      [x, y, w, h] -> pure (Just (fromIntegral x, fromIntegral y, fromIntegral w, fromIntegral h))
      _ -> pure Nothing
    else pure Nothing

createRenderer :: Window -> IO Renderer
createRenderer w = sdlCreateRenderer api w nullPtr >>= nonNull "SDL_CreateRenderer"

destroyRenderer :: Renderer -> IO ()
destroyRenderer = sdlDestroyRenderer api

setColor :: Renderer -> Color -> IO ()
setColor r (cr, cg, cb) = void (sdlSetRenderDrawColor api r (fromIntegral cr) (fromIntegral cg) (fromIntegral cb) 255)

renderClear, renderPresent :: Renderer -> IO Bool
renderClear r = toBool <$> sdlRenderClear api r
renderPresent r = toBool <$> sdlRenderPresent api r

fillRect :: Renderer -> Double -> Double -> Double -> Double -> IO ()
fillRect r x y w h = withArray (map realToFrac [x, y, w, h]) (void . sdlRenderFillRect api r)

renderDebugText :: Renderer -> Double -> Double -> String -> IO Bool
renderDebugText r x y s = withUtf8 s $ \cs -> toBool <$> sdlRenderDebugText api r (realToFrac x) (realToFrac y) cs

setRenderScale :: Renderer -> Double -> Double -> IO Bool
setRenderScale r sx sy = toBool <$> sdlSetRenderScale api r (realToFrac sx) (realToFrac sy)

data Event
  = Quit
  | MouseMotion Double Double
  | MouseDown Int Double Double
  | MouseUp Int Double Double
  | MouseLeave
  | Other
  deriving (Eq, Show)

-- SDL_Event is a 128-byte union; the fields read here sit at fixed offsets.
decode :: Ptr Word8 -> IO Event
decode buf = do
  kind <- peekByteOff buf 0 :: IO Word32
  let x = realToFrac <$> (peekByteOff buf 28 :: IO CFloat)
      y = realToFrac <$> (peekByteOff buf 32 :: IO CFloat)
      button = fromIntegral <$> (peekByteOff buf 24 :: IO Word8)
  if
    | kind == eventQuit || kind == eventWindowCloseRequested -> pure Quit
    | kind == eventMouseMotion -> MouseMotion <$> x <*> y
    | kind == eventMouseButtonDown -> MouseDown <$> button <*> x <*> y
    | kind == eventMouseButtonUp -> MouseUp <$> button <*> x <*> y
    | kind == eventWindowMouseLeave -> pure MouseLeave
    | otherwise -> pure Other

-- | Blocks up to @ms@ milliseconds; other Haskell threads run meanwhile.
waitEvent :: Int -> IO (Maybe Event)
waitEvent ms = allocaBytes 128 $ \buf -> do
  got <- sdlWaitEventTimeout api buf (fromIntegral ms)
  if toBool got then Just <$> decode buf else pure Nothing

-- | Drains queued events without blocking.
pollEvents :: IO [Event]
pollEvents = waitEvent 0 >>= maybe (pure []) (\e -> (e :) <$> pollEvents)

-- | Global pointer position and whether the left button (bit 0) is held.
globalMouse :: IO (Double, Double, Bool)
globalMouse = alloca $ \px -> alloca $ \py -> do
  buttons <- sdlGetGlobalMouseState api px py
  x <- peek px
  y <- peek py
  pure (realToFrac x, realToFrac y, buttons .&. 1 /= 0)

createSystemCursor :: Int -> IO Cursor
createSystemCursor = sdlCreateSystemCursor api . fromIntegral

setCursor :: Cursor -> IO Bool
setCursor c = toBool <$> sdlSetCursor api c

openUrl :: String -> IO Bool
openUrl u = withUtf8 u $ \cu -> toBool <$> sdlOpenURL api cu

-- | The NSWindow behind an SDL window; Nothing on other platforms or drivers.
cocoaWindow :: Window -> IO (Maybe (Ptr ()))
cocoaWindow w = do
  props <- sdlGetWindowProperties api w
  p <- withUtf8 "SDL.window.cocoa.window" $ \k -> sdlGetPointerProperty api props k nullPtr
  pure (if p == nullPtr then Nothing else Just p)

-- | Clips drawing to (x, y, w, h) in current render coordinates; Nothing clears.
setClip :: Renderer -> Maybe (Double, Double, Double, Double) -> IO ()
setClip r Nothing = void (sdlSetRenderClipRect api r nullPtr)
setClip r (Just (x, y, w, h)) =
  withArray (map (truncate . roundf) [x, y, w, h]) (void . sdlSetRenderClipRect api r)
