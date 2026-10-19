-- | Mirrors work items onto the first connected Stream Deck: one key per item,
-- orange while in progress, green when done, titled with the item name.
-- Short press opens the URL; long press deletes the item.
module Deck (start) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM_, forever, unless, void, when)
import Data.Array (Array, listArray, (!))
import qualified Data.Array as Array
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.IORef
import Data.Time.Clock.POSIX (getPOSIXTime)
import Ffi
import Foreign hiding (void)
import Foreign.C
import qualified Gui
import qualified Sdl
import System.Directory (doesFileExist)
import System.Hardware.StreamDeck (Image (..), Kind (..), StreamDeck, Update (..))
import qualified System.Hardware.StreamDeck as SD
import qualified System.Hardware.StreamDeck.Image as Image
import System.IO (IOMode (..), withFile)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)
import qualified Text
import Text.Printf (printf)
import qualified Wip.Color as Color
import Wip.AtExit (atExit)
import qualified Wip.DeckLogic as DL
import qualified Wip.Hyphen as Hyphen
import Wip.Layout (roundf)
import Wip.Store (Item (..), Status (..), Store)
import qualified Wip.Store as Store

logLine :: String -> IO ()
logLine s = putStrLn ("deck: " ++ s)

-- Label rendering: SDL3_ttf wraps the title, then it is blended onto a
-- coloured key image.

data Label = Label
  { api :: Text.TtfApi
  , font :: Ptr ()
  , maxPx :: Double
  , render :: Ptr () -> CString -> CSize -> Word32 -> CInt -> IO (Ptr ())
  , sizeWrapped :: Ptr () -> CString -> CSize -> CInt -> Ptr CInt -> Ptr CInt -> IO CBool
  }

rgba32 :: Word32
rgba32 = 0x16762004

alignCenter :: CInt
alignCenter = 1

pad :: Int
pad = 6

-- SDL_Color is four bytes passed by value; the C calling conventions on
-- arm64 and x86_64 pass it in one register, so it travels packed in a Word32.
sdlColor :: Color.Color -> Word32
sdlColor (r, g, b) =
  fromIntegral r .|. (fromIntegral g `shiftL` 8) .|. (fromIntegral b `shiftL` 16) .|. (255 `shiftL` 24)

convertSurface :: Ptr () -> Word32 -> IO (Ptr ())
convertSurface s f = Sdl.symbol "SDL_ConvertSurface" >>= \fp -> call_pu32_p fp s f

destroySurface :: Ptr () -> IO ()
destroySurface s = Sdl.symbol "SDL_DestroySurface" >>= \fp -> call_p_v fp s

createLabel :: Maybe FilePath -> Double -> IO (Maybe Label)
createLabel path px =
  Text.openFont path (roundf px) >>= \case
    Nothing -> pure Nothing
    Just (a, f, _) -> case Text.ttfLib of
      Nothing -> pure Nothing
      Just dl -> do
        setAlign <- call_pi_v <$> symbol dl "TTF_SetFontWrapAlignment"
        setAlign f alignCenter
        r <- call_pszu32i_p <$> symbol dl "TTF_RenderText_Blended_Wrapped"
        sw <- call_pszipp_b <$> symbol dl "TTF_GetStringSizeWrapped"
        pure (Just (Label a f px r sw))

floorPx :: Double
floorPx = 7

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, []) -> [a]
  (a, _ : rest) -> a : splitOn c rest

joinWith :: String -> [String] -> String
joinWith sep = foldr1 (\a b -> a ++ sep ++ b)

-- | Sets the font size so @text@ fits a @w@ x @h@ box and returns the text to
-- draw. @wholeLines@: every line must fit unwrapped (info screen).
-- Otherwise text wraps between words; the font shrinks down to @minPx@
-- keeping words whole, and below that words too wide are hyphenated at
-- syllables onto separate lines.
fit :: Bool -> Double -> Label -> Int -> Int -> String -> IO String
fit wholeLines minPx t w h text = do
  let innerW = w - (2 * pad)
      innerH = h - (2 * pad)
      width s = alloca $ \pw -> alloca $ \ph -> withUtf8 s $ \cs -> do
        poke pw 0
        void (Text.ttfStringSize (api t) (font t) cs 0 pw ph)
        fromIntegral <$> peek pw
      wrappedHeight s = alloca $ \pw -> alloca $ \ph -> withUtf8 s $ \cs -> do
        poke ph 0
        void (sizeWrapped t (font t) cs 0 (fromIntegral innerW) pw ph)
        fromIntegral <$> peek ph
      fitsW s = (<= innerW) <$> width s
      allM f = fmap and . mapM f
      ls = splitOn '\n' text
      ws = concatMap (splitOn ' ') ls
      hyphenated =
        joinWith "\n" <$> mapM (fmap (joinWith " ") . mapM hyphenWord . splitOn ' ') ls
      hyphenWord wd = do
        fitsWhole <- fitsW wd
        if fitsWhole then pure wd else joinWith "-\n" <$> Hyphen.splitM fitsW wd
      set px = void (Text.ttfSetFontSize (api t) (font t) (realToFrac px))
      shrink px fits = do
        set px
        good <- fits
        unless (px <= floorPx || good) (shrink (px - 0.5) fits)
  if wholeLines
    then do
      shrink (maxPx t) $ do
        widths <- allM fitsW ls
        lineH <- fromIntegral <$> Text.ttfFontHeight (api t) (font t)
        pure (widths && length ls * lineH <= innerH)
      pure text
    else do
      let whole = do
            wordsFit <- allM fitsW ws
            if wordsFit then (<= innerH) <$> wrappedHeight text else pure False
          keepWords px = do
            set px
            good <- whole
            if good then pure (Just text) else if px - 0.5 < minPx then pure Nothing else keepWords (px - 0.5)
          hyphenate px = do
            set px
            s <- hyphenated
            hgt <- wrappedHeight s
            if px <= floorPx || hgt <= innerH then pure s else hyphenate (px - 0.5)
      keepWords (maxPx t) >>= maybe (hyphenate (max floorPx minPx)) pure

-- | Alpha-blends wrapped @text@, centred, onto @img@.
blendText :: Label -> Image -> String -> Color.Color -> IO Image
blendText t img text fg@(tr, tg, tb) = do
  let (iw, ih) = Image.dimensions img
  surface <- withUtf8 (if null text then " " else text) $ \cs ->
    render t (font t) cs 0 (sdlColor fg) (fromIntegral (iw - (2 * pad)))
  if surface == nullPtr
    then pure img
    else do
      conv <- convertSurface surface rgba32
      destroySurface surface
      if conv == nullPtr
        then pure img
        else do
          w <- fromIntegral <$> (peekByteOff conv 8 :: IO CInt)
          h <- fromIntegral <$> (peekByteOff conv 12 :: IO CInt)
          pitch <- fromIntegral <$> (peekByteOff conv 16 :: IO CInt)
          pixels <- peekByteOff conv 24 :: IO (Ptr Word8)
          let visibleH = min h (ih - (2 * pad))
          bytes <- B.packCStringLen (castPtr pixels, pitch * max 0 visibleH)
          destroySurface conv
          let x0 = (iw - w) `div` 2
              y0 = (ih - visibleH) `div` 2
              mix c b a = fromIntegral ((fromIntegral c * a + fromIntegral b * (255 - a)) `div` 255 :: Int)
          pure . Image.generate iw ih $ \px py ->
            let base@(br, bg, bb) = Image.getPixel img px py
                x = px - x0
                y = py - y0
                a = fromIntegral (BU.unsafeIndex bytes ((y * pitch) + (x * 4) + 3)) :: Int
             in if x >= 0 && x < w && y >= 0 && y < visibleH && a > 0
                  then (mix tr br a, mix tg bg a, mix tb bb a)
                  else base

labelImage :: Maybe Label -> Int -> Int -> Color.Color -> Color.Color -> String -> IO Image
labelImage t w h (br, bg, bb) fg text = do
  let img = Image.fill (Image.create w h) (fromIntegral br, fromIntegral bg, fromIntegral bb)
  maybe (pure img) (\l -> blendText l img text fg) t

navBg, infoBg :: Color.Color
navBg = (0x2B, 0x2B, 0x2B)
infoBg = (0x10, 0x10, 0x10)

openUrl :: String -> IO ()
openUrl url = unless (null url) . void . forkIO $ do
  mac <- doesFileExist "/usr/bin/open"
  let opener = if mac then "/usr/bin/open" else "xdg-open"
  void . (try :: IO () -> IO (Either SomeException ())) $
    withFile "/dev/null" WriteMode $ \null' -> do
      (_, _, _, p) <- createProcess (proc opener [url]) {std_out = UseHandle null', std_err = UseHandle null'}
      void (waitForProcess p)

act :: Store -> DL.Action -> IO ()
act store = \case
  DL.Open i -> Store.find store i >>= mapM_ (openUrl . itemUrl)
  DL.Delete i -> void (Store.delete store i)

-- | Device in use, so the exit hook can blank the keys. Writes hold the lock.
current :: MVar (Maybe StreamDeck)
current = unsafePerformIO (newMVar Nothing)
{-# NOINLINE current #-}

withDevice :: IO a -> IO a
withDevice act' = withMVar current (const act')

ok :: IO (Either SD.Error a) -> IO a
ok m = m >>= either (fail . SD.errorToString) pure

now :: IO Double
now = realToFrac <$> getPOSIXTime

type Shown = Maybe (String, Color.Color, Color.Color)

runDevice :: Store -> Maybe FilePath -> StreamDeck -> Kind -> IO ()
runDevice store fontPath device kind = do
  let keys = SD.keyCount kind
      size = fst (SD.formatSize (SD.keyImageFormat kind))
      touchNav = SD.touchpointCount kind >= 2
  label <- createLabel fontPath (fromIntegral size * 0.18)
  when (null label) (logLine "no font, keys show colour only")
  -- Neo only: its info screen shows counts and the page.
  infoScreen <- case (kind, SD.lcdImageFormat kind) of
    (Neo, Just format) -> do
      let (w, h) = SD.formatSize format
      l <- createLabel fontPath (fromIntegral h * 0.3)
      pure (Just (format, w, h, l))
    _ -> pure Nothing
  shown <- newIORef (listArray (0, keys - 1) (replicate keys Nothing) :: Array Int Shown)
  shownInfo <- newIORef Nothing
  press <- newIORef DL.newPress
  reader <- SD.getReader device
  version <- newIORef (-1)
  page <- newIORef 0
  seenActivity <- newIORef 0
  slots <- newIORef (listArray (0, keys - 1) (replicate keys DL.Empty))
  withDevice (ok (SD.clearAllButtonImages device))
  let redraw items = do
        let p = DL.pages keys touchNav (length items)
        modifyIORef' page (DL.clampPage p)
        pg <- readIORef page
        let cur = DL.slots keys touchNav pg items
        writeIORef slots cur
        withDevice $ do
          forM_ (Array.assocs cur) $ \(k, slot) -> do
            let want = case slot of
                  DL.Empty -> Nothing
                  DL.ItemSlot i -> case itemColor i of
                    Nothing -> Just (itemTitle i, if itemStatus i == InProgress then Gui.orange else Gui.green, Gui.textDark)
                    Just c -> Just (itemTitle i, c, if Color.isLight c then Gui.textDark else Gui.textLight)
                  DL.Nav n count -> Just (printf "%d/%d\n>>" (n + 1) count, navBg, Gui.textLight)
            old <- (! k) <$> readIORef shown
            when (want /= old) $ do
              modifyIORef' shown (Array.// [(k, want)])
              case want of
                Nothing -> ok (SD.clearButtonImage device k)
                Just (text, bg, fg) -> do
                  text' <- maybe (pure text) (\l -> fit False (fromIntegral size * 0.11) l size size text) label
                  img <- labelImage label size size bg fg text'
                  ok (SD.setButtonImage device k img)
          ok (SD.flush device)
          forM_ infoScreen $ \(format, w, h, l) -> do
            let text = DL.infoText items pg (DL.count p)
            prev <- readIORef shownInfo
            when (Just text /= prev) $ do
              writeIORef shownInfo (Just text)
              mapM_ (\f -> void (fit True floorPx f w h text)) l
              img <- labelImage l w h infoBg Gui.textLight text
              bytes <- ok (SD.convertImageWithFormat format img)
              ok (SD.writeLcdFill device bytes)
      turn f = do
        n <- length <$> Store.items store
        modifyIORef' page (f (DL.pages keys touchNav n))
        writeIORef version (-1)
  forever $ do
    (v, items0) <- Store.snapshot store
    seen <- readIORef version
    when (v /= seen) $ do
      writeIORef version v
      let items = DL.byActivity items0
      -- New activity (add or status change) brings page 1 back.
      let latest = DL.latestActivity items
      prevLatest <- readIORef seenActivity
      when (latest > prevLatest) $ do
        writeIORef seenActivity latest
        writeIORef page 0
      redraw items
    updates <- ok (SD.readerRead reader (Just 0.05))
    t <- now
    forM_ updates $ \case
      ButtonDown k | k < keys -> do
        cur <- readIORef slots
        case cur ! k of
          DL.ItemSlot i -> modifyIORef' press (DL.down k (Just (itemId i)) t)
          DL.Nav _ _ -> turn DL.cycle
          DL.Empty -> pure ()
      ButtonUp k -> do
        action <- atomicModifyIORef' press (\p -> let (a, p') = DL.up k t p in (p', a))
        mapM_ (act store) action
      TouchPointDown 0 | touchNav -> turn DL.back
      TouchPointDown 1 | touchNav -> turn DL.forward
      _ -> pure ()
    actions <- atomicModifyIORef' press (\p -> let (as, p') = DL.tick t p in (p', as))
    mapM_ (act store) actions

-- | Finds a device, drives it until it fails or unplugs, then looks again.
loop :: Store -> Maybe FilePath -> IO ()
loop store fontPath =
  (try SD.newHidApi :: IO (Either SomeException (Either String SD.HidApi))) >>= \case
    Left e -> logLine ("hidapi unavailable: " ++ show e)
    Right (Left e) -> logLine ("hidapi unavailable: " ++ e)
    Right (Right hid) -> do
      waiting <- newIORef False
      forever $ do
        void (SD.refreshDeviceList hid)
        SD.listDevices hid >>= \case
          [] -> do
            w <- readIORef waiting
            unless w (logLine "no Stream Deck found, waiting")
            writeIORef waiting True
            threadDelay 3000000
          (kind, serial) : _ -> do
            writeIORef waiting False
            SD.connect hid kind serial >>= \case
              Left e -> do
                logLine ("cannot open " ++ serial ++ ": " ++ SD.errorToString e)
                threadDelay 3000000
              Right device -> do
                logLine ("using " ++ SD.kindToString kind ++ " " ++ serial)
                modifyMVar_ current (const (pure (Just device)))
                r <- try (runDevice store fontPath device kind)
                case r of
                  Left (e :: SomeException) -> logLine ("device lost: " ++ show e)
                  Right () -> pure ()
                modifyMVar_ current (const (Nothing <$ SD.close device))
                threadDelay 1000000

start :: Store -> Maybe FilePath -> IO ()
start store fontPath = do
  void (evaluate Text.ttfApi)
  atExit . withMVar current . mapM_ $ \d -> do
    void (SD.clearAllButtonImages d)
    case (SD.kind d, SD.lcdImageFormat (SD.kind d)) of
      (Neo, Just format) -> do
        let (w, h) = SD.formatSize format
        SD.convertImageWithFormat format (Image.create w h) >>= mapM_ (void . SD.writeLcdFill d)
      _ -> pure ()
  void (forkIO (loop store fontPath))
