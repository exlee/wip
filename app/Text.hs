-- | Text drawing: a TrueType monospace font through SDL3_ttf, or SDL's 8x8
-- debug font when SDL3_ttf or the font file is missing.
module Text
  ( TtfApi (..)
  , ttfLib
  , ttfApi
  , Font
  , openFont
  , load
  , charW
  , lineH
  , draw
  ) where

import Control.Monad (void, when)
import Data.IORef
import Ffi
import Foreign hiding (void)
import Foreign.C
import Sdl (Renderer)
import qualified Sdl
import System.Directory (doesFileExist)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.DynamicLinker (RTLDFlags (..))
import Wip.Color (Color)
import Wip.Layout (roundf)

ttfLib :: Maybe DL
ttfLib =
  unsafePerformIO $
    openFirst
      [RTLD_NOW]
      [ "libSDL3_ttf.0.dylib"
      , "/opt/homebrew/lib/libSDL3_ttf.0.dylib"
      , "/usr/local/lib/libSDL3_ttf.0.dylib"
      , "libSDL3_ttf.so.0"
      ]
{-# NOINLINE ttfLib #-}

data TtfApi = TtfApi
  { ttfInit :: IO CBool
  , ttfOpenFont :: CString -> CFloat -> IO (Ptr ())
  , ttfSetFontSize :: Ptr () -> CFloat -> IO CBool
  , ttfFontHeight :: Ptr () -> IO CInt
  , ttfStringSize :: Ptr () -> CString -> CSize -> Ptr CInt -> Ptr CInt -> IO CBool
  , ttfCreateEngine :: Ptr () -> IO (Ptr ())
  , ttfCreateText :: Ptr () -> Ptr () -> CString -> CSize -> IO (Ptr ())
  , ttfSetTextColor :: Ptr () -> Word8 -> Word8 -> Word8 -> Word8 -> IO CBool
  , ttfDrawText :: Ptr () -> CFloat -> CFloat -> IO CBool
  , ttfDestroyText :: Ptr () -> IO ()
  }

ttfApi :: Maybe TtfApi
ttfApi = unsafePerformIO $ case ttfLib of
  Nothing -> pure Nothing
  Just dl -> do
    let fn mk name = mk <$> symbol dl name
    fmap Just $
      TtfApi
        <$> fn call_v_b "TTF_Init"
        <*> fn call_sf_p "TTF_OpenFont"
        <*> fn call_pf_b "TTF_SetFontSize"
        <*> fn call_p_i "TTF_GetFontHeight"
        <*> fn call_pszpp_b "TTF_GetStringSize"
        <*> fn call_p_p "TTF_CreateRendererTextEngine"
        <*> fn call_pps_p "TTF_CreateText"
        <*> fn call_pu8x4_b "TTF_SetTextColor"
        <*> fn call_pff_b "TTF_DrawRendererText"
        <*> fn call_p_v "TTF_DestroyText"
{-# NOINLINE ttfApi #-}

data Metrics = Metrics
  { density :: Double
  , mCharW :: Double -- points
  , mLineH :: Double -- points
  }

data Ttf = Ttf
  { api :: TtfApi
  , font :: Ptr ()
  , engine :: Ptr ()
  , sizePt :: Double
  , metrics :: IORef Metrics
  }

data Font = TtfFont Ttf | Debug

debugScale :: Double
debugScale = 1.5

defaultFonts :: [FilePath]
defaultFonts =
  [ "/System/Library/Fonts/SFNSMono.ttf"
  , "/System/Library/Fonts/Menlo.ttc"
  , "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"
  ]

-- | Font size follows the pixel density so glyphs stay sharp on Retina screens.
setDensity :: Ttf -> Double -> IO ()
setDensity t d = do
  m <- readIORef (metrics t)
  when (d /= density m) $ do
    void (ttfSetFontSize (api t) (font t) (realToFrac (sizePt t * d)))
    w <- alloca $ \pw -> alloca $ \ph -> withUtf8 "M" $ \s -> do
      poke pw 0
      void (ttfStringSize (api t) (font t) s 1 pw ph)
      peek pw
    h <- ttfFontHeight (api t) (font t)
    writeIORef (metrics t) (Metrics d (fromIntegral w / d) (fromIntegral h / d))

-- | Opens the first usable font at @size@ points: @path@ if given, else the defaults.
openFont :: Maybe FilePath -> Double -> IO (Maybe (TtfApi, Ptr (), FilePath))
openFont path size = case ttfApi of
  Nothing -> pure Nothing
  Just a -> do
    ok <- toBool <$> ttfInit a
    if not ok then pure Nothing else firstJust a (maybe defaultFonts pure path)
 where
  firstJust _ [] = pure Nothing
  firstJust a (p : ps) = do
    exists <- doesFileExist p
    f <- if exists then withUtf8 p (\cp -> ttfOpenFont a cp (realToFrac size)) else pure nullPtr
    if f == nullPtr then firstJust a ps else pure (Just (a, f, p))

load :: Renderer -> Maybe FilePath -> Double -> IO Font
load renderer path size =
  openFont path size >>= \case
    Nothing -> do
      hPutStrLn stderr "wip: SDL3_ttf or font unavailable, using the built-in bitmap font"
      pure Debug
    Just (a, f, p) -> do
      putStrLn ("Font: " ++ p)
      e <- ttfCreateEngine a renderer
      m <- newIORef (Metrics 0 0 0)
      let t = Ttf a f e size m
      setDensity t 1
      pure (TtfFont t)

charW, lineH :: Font -> IO Double
charW (TtfFont t) = mCharW <$> readIORef (metrics t)
charW Debug = pure (8 * debugScale)
lineH (TtfFont t) = mLineH <$> readIORef (metrics t)
lineH Debug = pure (8 * debugScale)

-- | Draws @s@ at (x, y) points. @clip@ is (x, y, w, h) in points; text
-- outside it is not drawn.
draw :: Maybe (Double, Double, Double, Double) -> Font -> Renderer -> Double -> (Double, Double) -> Color -> String -> IO ()
draw clip t r dens (x, y) color@(cr, cg, cb) s = case t of
  Debug -> do
    Sdl.setColor r color
    let k = dens * debugScale
    void (Sdl.setRenderScale r k k)
    setClip debugScale
    void (Sdl.renderDebugText r (x / debugScale) (y / debugScale) s)
    Sdl.setClip r Nothing
    void (Sdl.setRenderScale r dens dens)
  TtfFont f -> do
    setDensity f dens
    -- Draw in device pixels at whole coordinates so glyphs are not resampled.
    void (Sdl.setRenderScale r 1 1)
    setClip (1 / dens)
    text <- withUtf8 s $ \cs -> ttfCreateText (api f) (engine f) (font f) cs 0
    when (text /= nullPtr) $ do
      void (ttfSetTextColor (api f) text (fromIntegral cr) (fromIntegral cg) (fromIntegral cb) 255)
      void (ttfDrawText (api f) text (realToFrac (roundf (x * dens))) (realToFrac (roundf (y * dens))))
      ttfDestroyText (api f) text
    Sdl.setClip r Nothing
    void (Sdl.setRenderScale r dens dens)
 where
  setClip k = Sdl.setClip r (fmap (\(cx, cy, cw, ch) -> (cx / k, cy / k, cw / k, ch / k)) clip)
