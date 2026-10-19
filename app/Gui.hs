-- | SDL window that draws the work item list.
module Gui
  ( orange
  , orangeHover
  , green
  , greenHover
  , textLight
  , textDark
  , runGui
  ) where

import Control.Monad (forM_, void, when)
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, isNothing)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Macos (floatOverEverything, statusLevel)
import Sdl (Event (..))
import qualified Sdl
import Text (Font)
import qualified Text
import Text.Printf (printf)
import qualified Wip.Color as Color
import Wip.Layout
import Wip.Store (Item (..), Status (..), Store)
import qualified Wip.Store as Store

orange, orangeHover, green, greenHover, background, headerBg, textLight, textDim, textDark, closeHover :: Color.Color
orange = (0xF7, 0x8C, 0x1F)
orangeHover = (0xFF, 0xA8, 0x4A)
green = (0x3F, 0xB9, 0x50)
greenHover = (0x62, 0xD1, 0x70)
background = (0x1E, 0x1E, 0x1E)
headerBg = (0x2B, 0x2B, 0x2B)
textLight = (0xE6, 0xE6, 0xE6)
textDim = (0x8A, 0x8A, 0x8A)
textDark = (0x14, 0x14, 0x14)
closeHover = (0xE5, 0x48, 0x4D)

statusStrip :: Double
statusStrip = 6

-- | Returns True while some title is scrolling, so the caller keeps redrawing.
-- @elapsed id@ is seconds since item @id@ appeared.
draw :: Sdl.Renderer -> Font -> Double -> Int -> Hit -> (String -> IO Double) -> [Item] -> IO Bool
draw r font density port hover elapsed items = do
  let drawText clip = Text.draw clip font r density
  charW <- Text.charW font
  lineH <- Text.lineH font
  let textY top h = roundf (top + ((h - lineH) / 2))
  void (Sdl.setRenderScale r density density)
  Sdl.setColor r background
  void (Sdl.renderClear r)
  -- header
  Sdl.setColor r headerBg
  Sdl.fillRect r 0 0 width headerH
  let doneCount = length (filter ((== Done) . itemStatus) items)
  drawText Nothing (8, textY 0 headerH) textLight (printf "WIP %d/%d  :%d" doneCount (length items) port)
  when (hover == Close) $ do
    Sdl.setColor r closeHover
    Sdl.fillRect r (width - closeW) 0 closeW headerH
  drawText Nothing (width - closeW + ((closeW - charW) / 2), textY 0 headerH) textLight "x"
  -- rows
  scrolling <-
    if null items
      then False <$ drawText Nothing (8, textY headerH rowH) textDim "no items"
      else fmap or . sequence $ zipWith (drawRow drawText charW textY) [0 ..] items
  void (Sdl.renderPresent r)
  pure scrolling
 where
  drawRow drawText charW textY idx item = do
    let hovered = hover == Row idx
        statusBg = case itemStatus item of
          InProgress -> if hovered then orangeHover else orange
          Done -> if hovered then greenHover else green
        bg = maybe statusBg (\c -> if hovered then Color.lighten c else c) (itemColor item)
        fg = if Color.isLight bg then textDark else textLight
        y = rowY idx
    Sdl.setColor r bg
    Sdl.fillRect r margin (y + 2) (width - (2 * margin)) (rowH - 2)
    -- A custom background keeps the status visible as a strip on the left.
    let strip = if isNothing (itemColor item) then 0 else statusStrip
    when (strip > 0) $ do
      Sdl.setColor r statusBg
      Sdl.fillRect r margin (y + 2) strip (rowH - 2)
    let label = "#" ++ itemId item ++ " " ++ itemTitle item
        x = margin + strip + 6
        avail = width - (2 * margin) - strip - 12
        overflow = (fromIntegral (textLength label) * charW) - avail
    offset <- marqueeOffset overflow <$> elapsed (itemId item)
    () <- drawText (Just (x, y + 2, avail, rowH - 2)) (x - offset, textY (y + 2) (rowH - 2)) fg label
    pure (overflow > 0)

placeTopRight :: Sdl.Window -> IO ()
placeTopRight w =
  Sdl.primaryUsableBounds >>= \case
    Just (x, y, bw, _) -> void (Sdl.setWindowPosition w (x + bw - truncate width - 20) (y + 20))
    Nothing -> pure ()

now :: IO Double
now = realToFrac <$> getPOSIXTime

runGui :: Maybe FilePath -> Store -> Int -> IO ()
runGui fontPath store port = do
  -- Never take focus: no dock icon, no activation, clicks pass through.
  void (Sdl.setHint "SDL_MAC_BACKGROUND_APP" "1")
  void (Sdl.setHint "SDL_WINDOW_ACTIVATE_WHEN_SHOWN" "0")
  void (Sdl.setHint "SDL_MOUSE_FOCUS_CLICKTHROUGH" "1")
  Sdl.initVideo
  rows0 <- length <$> Store.items store
  w <-
    Sdl.createWindow
      "WIP"
      (truncate width)
      (truncate (height rows0))
      [Sdl.windowBorderless, Sdl.windowAlwaysOnTop, Sdl.windowUtility, Sdl.windowNotFocusable, Sdl.windowHighPixelDensity]
  placeTopRight w
  Sdl.cocoaWindow w >>= mapM_ (floatOverEverything statusLevel)
  r <- Sdl.createRenderer w
  font <- Text.load r fontPath 13
  cursorDefault <- Sdl.createSystemCursor Sdl.cursorDefault
  cursorPointer <- Sdl.createSystemCursor Sdl.cursorPointer
  cursorMove <- Sdl.createSystemCursor Sdl.cursorMove
  hover <- newIORef NoHit
  pressed <- newIORef NoHit
  drag <- newIORef Nothing
  shownVersion <- newIORef (-1)
  shownRows <- newIORef rows0
  running <- newIORef True
  scrolling <- newIORef False
  firstSeen <- newIORef Map.empty
  let elapsed i = do
        t <- now
        seen <- readIORef firstSeen
        case Map.lookup i seen of
          Just t0 -> pure (t - t0)
          Nothing -> 0 <$ modifyIORef' firstSeen (Map.insert i t)
      setHover h = do
        old <- readIORef hover
        when (h /= old) $ do
          writeIORef hover h
          cursor <- case h of
            Row i -> do
              is <- Store.items store
              pure $ case drop i is of
                item : _ | itemUrl item /= "" -> cursorPointer
                _ -> cursorDefault
            Close -> pure cursorPointer
            Header -> pure cursorMove
            NoHit -> pure cursorDefault
          void (Sdl.setCursor cursor)
          writeIORef shownVersion (-1)
      handle rows items = \case
        Quit -> writeIORef running False
        MouseMotion x y -> setHover (hit rows x y)
        MouseLeave -> setHover NoHit
        MouseDown 1 x y -> do
          let h = hit rows x y
          writeIORef pressed h
          when (h == Header) $ do
            (gx, gy, _) <- Sdl.globalMouse
            (wx, wy) <- Sdl.getWindowPosition w
            writeIORef drag (Just (gx - fromIntegral wx, gy - fromIntegral wy))
        MouseUp 1 x y -> do
          let h = hit rows x y
          p <- readIORef pressed
          when (h == p) $ case h of
            Close -> writeIORef running False
            Row i -> case drop i items of
              item : _ | itemUrl item /= "" -> void (Sdl.openUrl (itemUrl item))
              _ -> pure ()
            _ -> pure ()
          writeIORef pressed NoHit
          writeIORef drag Nothing
        _ -> pure ()
      loop = do
        dragging <- isJust <$> readIORef drag
        scroll <- readIORef scrolling
        first <- Sdl.waitEvent (if dragging then 16 else if scroll then 33 else 100)
        events <- (maybe [] pure first ++) <$> Sdl.pollEvents
        (_, items) <- Store.snapshot store
        forM_ events (handle (length items) items)
        readIORef drag >>= \case
          Just (dx, dy) -> do
            (gx, gy, held) <- Sdl.globalMouse
            if held
              then void (Sdl.setWindowPosition w (truncate (gx - dx)) (truncate (gy - dy)))
              else writeIORef drag Nothing
          Nothing -> pure ()
        (version, items') <- Store.snapshot store
        let rows = length items'
        shown <- readIORef shownRows
        when (rows /= shown) $ do
          writeIORef shownRows rows
          void (Sdl.setWindowSize w (truncate width) (truncate (height rows)))
        sv <- readIORef shownVersion
        scroll' <- readIORef scrolling
        when (version /= sv || not (null events) || scroll') $ do
          writeIORef shownVersion version
          density <- Sdl.getWindowPixelDensity w
          h <- readIORef hover
          draw r font density port h elapsed items' >>= writeIORef scrolling
        stillRunning <- readIORef running
        when stillRunning loop
  loop
  Sdl.destroyRenderer r
  Sdl.destroyWindow w
  Sdl.quit
