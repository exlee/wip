-- | Stream Deck behaviour independent of the device: which item sits on which
-- key, paging, and how presses turn into actions.
module Wip.DeckLogic
  ( byActivity
  , latestActivity
  , Pages (..)
  , pages
  , clampPage
  , forward
  , back
  , cycle
  , Slot (..)
  , slots
  , counts
  , infoText
  , Action (..)
  , Press
  , newPress
  , newPressWith
  , down
  , tick
  , up
  ) where

import Data.Array (Array, accumArray, (//))
import Data.List (find, sortOn)
import Data.Ord (Down (..))
import Text.Printf (printf)
import Wip.Store (Item (..), Status (..))
import Prelude hiding (cycle)

-- | Most recent activity first, so fresh items land on page 1.
byActivity :: [Item] -> [Item]
byActivity = sortOn (Down . itemActivity)

latestActivity :: [Item] -> Int
latestActivity = foldl (\m i -> max m (itemActivity i)) 0

-- | Page geometry. Without touch buttons for paging, the last key turns into a
-- "next page" key once the items do not fit.
data Pages = Pages {perPage :: Int, count :: Int, navKey :: Maybe Int}
  deriving (Eq, Show)

pages :: Int -> Bool -> Int -> Pages
pages keys touchNav n =
  Pages
    { perPage = pp
    , count = max 1 ((n + pp - 1) `div` pp)
    , navKey = if nav then Just (keys - 1) else Nothing
    }
 where
  nav = not touchNav && n > keys && keys > 1
  pp = if nav then keys - 1 else keys

clampPage :: Pages -> Int -> Int
clampPage p page = max 0 (min page (count p - 1))

-- | Touch buttons step back and forward and stop at the ends; the nav key cycles.
forward, back, cycle :: Pages -> Int -> Int
forward p page = clampPage p (page + 1)
back p page = clampPage p (page - 1)
cycle p page = (page + 1) `mod` count p

data Slot = Empty | ItemSlot Item | Nav Int Int -- page, page count
  deriving (Eq, Show)

-- | What each key shows on @page@; @items@ already in display order.
slots :: Int -> Bool -> Int -> [Item] -> Array Int Slot
slots keys touchNav page0 items =
  base // maybe [] (\k -> [(k, Nav page (count p))]) (navKey p)
 where
  p = pages keys touchNav (length items)
  page = clampPage p page0
  placed =
    [ (k, ItemSlot item)
    | (i, item) <- zip [0 ..] items
    , let k = i - (page * perPage p)
    , k >= 0 && k < perPage p
    ]
  base = accumArray (\_ s -> s) Empty (0, keys - 1) placed

-- | (working, done, total)
counts :: [Item] -> (Int, Int, Int)
counts items = (total - done, done, total)
 where
  done = length (filter ((== Done) . itemStatus) items)
  total = length items

infoText :: [Item] -> Int -> Int -> String
infoText items page pageCount =
  let (working, done, total) = counts items
   in printf "Working %d / Done %d / Total %d\nPage %d/%d" working done total (page + 1) pageCount

data Action = Open String | Delete String -- item ids
  deriving (Eq, Show)

data Held = Held {key :: Int, heldItem :: Maybe String, started :: Double, fired :: Bool}

-- | Keys currently held down; long presses act after 'longAfter' seconds.
data Press = Press {longAfter :: Double, pressed :: [Held]}

newPress :: Press
newPress = newPressWith 0.6

newPressWith :: Double -> Press
newPressWith l = Press l []

down :: Int -> Maybe String -> Double -> Press -> Press
down k item now t = t {pressed = Held k item now False : filter ((/= k) . key) (pressed t)}

-- | Long presses act while the key is still held, once.
tick :: Double -> Press -> ([Action], Press)
tick now t = ([Delete i | h <- due, Just i <- [heldItem h]], t {pressed = map mark (pressed t)})
 where
  isDue h = not (fired h) && now - started h >= longAfter t
  due = filter isDue (pressed t)
  mark h = if isDue h then h {fired = True} else h

up :: Int -> Double -> Press -> (Maybe Action, Press)
up k now t = case find ((== k) . key) (pressed t) of
  Nothing -> (Nothing, t)
  Just h ->
    let t' = t {pressed = filter ((/= k) . key) (pressed t)}
        action
          | fired h = Nothing
          | now - started h >= longAfter t = Delete <$> heldItem h
          | otherwise = Open <$> heldItem h
     in (action, t')
