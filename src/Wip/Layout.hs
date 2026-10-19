-- | Window geometry in points, independent of SDL so it can be tested.
module Wip.Layout
  ( width
  , headerH
  , rowH
  , margin
  , closeW
  , Hit (..)
  , height
  , rowY
  , hit
  , fitText
  , textLength
  , Marquee (..)
  , defaultMarquee
  , marqueeOffset
  , marqueeOffsetWith
  , roundf
  , RowClick (..)
  , rowClick
  ) where

import Data.Fixed (mod')
import Wip.Store (Item (..))

width, headerH, rowH, margin, closeW :: Double
width = 360
headerH = 28
rowH = 30
margin = 4
closeW = 28

data Hit = NoHit | Header | Close | Row Int
  deriving (Eq, Show)

height :: Int -> Double
height rows = headerH + (fromIntegral (max rows 1) * rowH) + margin

rowY :: Int -> Double
rowY i = headerH + (fromIntegral i * rowH)

-- | What sits under (x, y) when @rows@ rows are shown.
hit :: Int -> Double -> Double -> Hit
hit rows x y
  | x < 0 || x >= width || y < 0 = NoHit
  | y < headerH = if x >= width - closeW then Close else Header
  | i < rows = Row i
  | otherwise = NoHit
 where
  i = truncate ((y - headerH) / rowH)

-- | Cuts @s@ so it fits @px@ points of a monospace font whose glyphs are
-- @charW@ wide, marking the cut with "..". Counts characters, so a cut never
-- splits a UTF-8 sequence.
fitText :: Double -> String -> Double -> String
fitText charW s px
  | length s <= maxChars = s
  | otherwise = take keep s ++ if maxChars <= 2 then "" else ".."
 where
  maxChars = truncate (px / charW)
  keep = max 0 (if maxChars <= 2 then maxChars else maxChars - 2)

-- | Length in characters (code points).
textLength :: String -> Int
textLength = length

data Marquee = Marquee {speed :: Double, holdStart :: Double, holdEnd :: Double}

defaultMarquee :: Marquee
defaultMarquee = Marquee {speed = 30, holdStart = 2, holdEnd = 1}

-- | 'marqueeOffsetWith' at the default speed and hold times.
marqueeOffset :: Double -> Double -> Double
marqueeOffset = marqueeOffsetWith defaultMarquee

-- | Horizontal offset in points for a title @overflow@ points wider than its
-- row, @t@ seconds after it appeared: hold at the start, scroll left until the
-- end is visible, hold at the end, jump back and repeat.
marqueeOffsetWith :: Marquee -> Double -> Double -> Double
marqueeOffsetWith m overflow t
  | overflow <= 0 = 0
  | p < holdStart m = 0
  | otherwise = min overflow ((p - holdStart m) * speed m)
 where
  scroll = overflow / speed m
  p = max 0 t `mod'` (holdStart m + scroll + holdEnd m)

-- | Rounds half away from zero, like C's round.
roundf :: Double -> Double
roundf x = if x < 0 then negate (fromInteger (floor (negate x + 0.5))) else fromInteger (floor (x + 0.5))

data RowClick = OpenUrl String | DeleteItem String | Ignore
  deriving (Eq, Show)

-- | What a click on @item@ does; @command@ is whether Cmd was held.
rowClick :: Bool -> Item -> RowClick
rowClick command item
  | command = DeleteItem (itemId item)
  | itemUrl item /= "" = OpenUrl (itemUrl item)
  | otherwise = Ignore
