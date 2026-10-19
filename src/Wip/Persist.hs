-- | Saves the store to a JSON file and loads it back on startup.
module Wip.Persist
  ( defaultPath
  , toJson
  , ofJson
  , save
  , load
  , autosave
  , autosaveEvery
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (IOException, try)
import Control.Monad (forever, void, when)
import qualified Data.ByteString as B
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.Environment (getEnv, lookupEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO (hPutStrLn, stderr)
import qualified Wip.Color as Color
import Wip.AtExit (atExit)
import Wip.Json
import Wip.Store (Dump (..), Item (..), Store)
import qualified Wip.Store as Store

defaultPath :: Int -> IO FilePath
defaultPath port = do
  xdg <- lookupEnv "XDG_STATE_HOME"
  base <- case xdg of
    Just d | not (null d) -> pure d
    _ -> (</> ".local/state") <$> getEnv "HOME"
  pure (base </> "wip" </> ("session-" ++ show port ++ ".json"))

toJson :: Dump -> Json
toJson d =
  Obj
    [ ("version", Num 1)
    , ("next_id", Num (fromIntegral (dumpNextId d)))
    , ("activity", Num (fromIntegral (dumpActivity d)))
    , ("items", Arr (map item (dumpItems d)))
    ]
 where
  item i =
    Obj $
      [ ("id", Str (itemId i))
      , ("title", Str (itemTitle i))
      , ("url", Str (itemUrl i))
      , ("status", Str (Store.statusToString (itemStatus i)))
      , ("activity", Num (fromIntegral (itemActivity i)))
      , ("strip", Bool (itemStrip i))
      ]
        ++ maybe [] (\c -> [("color", Str (Color.toHex c))]) (itemColor i)

ofJson :: Json -> Either String Dump
ofJson v =
  case (member "items" v, int "next_id" v, int "activity" v) of
    (Just (Arr is), Just nextId, Just activity) ->
      maybe (Left "malformed item") (\parsed -> Right (Dump parsed nextId activity)) (traverse item is)
    _ -> Left "missing items, next_id or activity"
 where
  int k o = toIntOpt (member k o)
  str k o = toStringOpt (member k o)
  item o = do
    i <- str "id" o
    title <- str "title" o
    status <- str "status" o >>= Store.statusOfString
    pure
      Item
        { itemId = i
        , itemTitle = title
        , itemUrl = fromMaybe "" (str "url" o)
        , itemStatus = status
        , itemColor = str "color" o >>= Color.parse
        , itemStrip = member "strip" o /= Just (Bool False)
        , itemActivity = fromMaybe 0 (int "activity" o)
        }

-- | Writes through a temporary file and rename, so a crash never leaves half a file.
save :: FilePath -> Store -> IO ()
save path store = do
  createDirectoryIfMissing True (takeDirectory path)
  let tmp = path ++ ".tmp"
  d <- Store.dump store
  B.writeFile tmp (TE.encodeUtf8 (T.pack (render (toJson d))))
  renameFile tmp path

-- | Right True: restored; Right False: no file yet. On a broken file the store
-- stays empty and the file is moved aside to @path.bad@.
load :: FilePath -> Store -> IO (Either String Bool)
load path store = do
  exists <- doesFileExist path
  if not exists
    then pure (Right False)
    else do
      raw <- try (B.readFile path)
      let result = case raw of
            Left (e :: IOException) -> Left (show e)
            Right bytes -> parse (T.unpack (TE.decodeUtf8Lenient bytes)) >>= ofJson
      case result of
        Right d -> Right True <$ Store.restore store d
        Left e -> do
          void (try (renameFile path (path ++ ".bad")) :: IO (Either IOException ()))
          pure (Left e)

-- | 'autosaveEvery' a quarter second.
autosave :: FilePath -> Store -> IO ()
autosave = autosaveEvery 0.25

-- | Saves shortly after every change, and once more at exit.
autosaveEvery :: Double -> FilePath -> Store -> IO ()
autosaveEvery interval path store = do
  saved <- newMVar =<< Store.version store
  let flush = modifyMVar_ saved $ \s -> do
        v <- Store.version store
        when (v /= s) $ do
          r <- try (save path store)
          case r of
            Left (e :: IOException) -> hPutStrLn stderr ("wip: cannot save session: " ++ show e)
            Right () -> pure ()
        pure v
  atExit flush
  void . forkIO . forever $ do
    threadDelay (round (interval * 1e6))
    flush
