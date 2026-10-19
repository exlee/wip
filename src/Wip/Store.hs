-- | Thread-safe list of work items shared by the GUI and the MCP server.
module Wip.Store
  ( Status (..)
  , Item (..)
  , Store
  , Dump (..)
  , statusToString
  , statusOfString
  , create
  , add
  , addNew
  , delete
  , move
  , setStatus
  , setColor
  , find
  , snapshot
  , items
  , dump
  , restore
  , version
  ) where

import Control.Concurrent.MVar
import qualified Data.List as List
import Wip.Color (Color)

data Status = InProgress | Done
  deriving (Eq, Show)

-- | 'itemActivity' grows on every add or status change; higher is more recent.
-- 'itemColor' overrides the status colour as the row background.
data Item = Item
  { itemId :: String
  , itemTitle :: String
  , itemUrl :: String
  , itemStatus :: Status
  , itemColor :: Maybe Color
  , itemActivity :: Int
  }
  deriving (Eq, Show)

data State = State
  { stItems :: [Item] -- insertion order
  , stNextId :: Int
  , stVersion :: Int
  , stActivity :: Int
  }

newtype Store = Store (MVar State)

statusToString :: Status -> String
statusToString InProgress = "in_progress"
statusToString Done = "done"

statusOfString :: String -> Maybe Status
statusOfString "in_progress" = Just InProgress
statusOfString "done" = Just Done
statusOfString _ = Nothing

create :: IO Store
create = Store <$> newMVar (State [] 1 0 0)

withState :: Store -> (State -> (State, a)) -> IO a
withState (Store mv) f = modifyMVar mv (pure . f)

readState :: Store -> (State -> a) -> IO a
readState (Store mv) f = f <$> readMVar mv

touch :: State -> State
touch st = st {stVersion = stVersion st + 1}

nextActivity :: State -> (State, Int)
nextActivity st = let a = stActivity st + 1 in (st {stActivity = a}, a)

replaceItem :: Item -> [Item] -> [Item]
replaceItem item = map (\i -> if itemId i == itemId item then item else i)

-- | Without an id the next free number is used. Adding an existing id updates
-- that item in place, so callers can re-add an id they own.
add :: Store -> Maybe String -> Maybe Color -> String -> String -> Status -> IO Item
add store mid color title url status = withState store $ \st0 ->
  let taken i = any ((== i) . itemId) (stItems st0)
      fresh st =
        let i = show (stNextId st)
            st' = st {stNextId = stNextId st + 1}
         in if taken i then fresh st' else (st', i)
      (st1, newId) = maybe (fresh st0) (st0,) mid
      (st2, act) = nextActivity st1
      item = Item newId title url status color act
      items'
        | taken newId = replaceItem item (stItems st2)
        | otherwise = stItems st2 ++ [item]
   in (touch st2 {stItems = items'}, item)

-- | 'add' with an automatic id and no colour.
addNew :: Store -> String -> String -> Status -> IO Item
addNew store = add store Nothing Nothing

delete :: Store -> String -> IO Bool
delete store i = withState store $ \st ->
  let kept = filter ((/= i) . itemId) (stItems st)
      removed = length kept < length (stItems st)
   in (if removed then touch st {stItems = kept} else st, removed)

-- | Moves item @i@ to position @to@ (clamped). Not activity: deck order stays put.
move :: Store -> String -> Int -> IO Bool
move store i to = withState store $ \st ->
  case List.partition ((== i) . itemId) (stItems st) of
    ([item], rest) ->
      let (before, after) = splitAt (max 0 to) rest
          items' = before ++ item : after
       in (if items' /= stItems st then touch st {stItems = items'} else st, True)
    _ -> (st, False)

setStatus :: Store -> String -> Status -> IO (Maybe Item)
setStatus store i status = withState store $ \st ->
  case List.find ((== i) . itemId) (stItems st) of
    Nothing -> (st, Nothing)
    Just old ->
      let (st', act) = nextActivity st
          item = old {itemStatus = status, itemActivity = act}
       in (touch st' {stItems = replaceItem item (stItems st')}, Just item)

-- | Colour changes do not count as activity: deck order stays put.
setColor :: Store -> String -> Maybe Color -> IO (Maybe Item)
setColor store i color = withState store $ \st ->
  case List.find ((== i) . itemId) (stItems st) of
    Nothing -> (st, Nothing)
    Just old ->
      let item = old {itemColor = color}
       in (touch st {stItems = replaceItem item (stItems st)}, Just item)

find :: Store -> String -> IO (Maybe Item)
find store i = readState store (List.find ((== i) . itemId) . stItems)

snapshot :: Store -> IO (Int, [Item])
snapshot store = readState store (\st -> (stVersion st, stItems st))

items :: Store -> IO [Item]
items store = snd <$> snapshot store

-- | Everything needed to rebuild the store: items, next auto id, activity counter.
data Dump = Dump {dumpItems :: [Item], dumpNextId :: Int, dumpActivity :: Int}
  deriving (Eq, Show)

dump :: Store -> IO Dump
dump store = readState store (\st -> Dump (stItems st) (stNextId st) (stActivity st))

restore :: Store -> Dump -> IO ()
restore store d = withState store $ \st ->
  (touch st {stItems = dumpItems d, stNextId = dumpNextId d, stActivity = dumpActivity d}, ())

version :: Store -> IO Int
version store = readState store stVersion
