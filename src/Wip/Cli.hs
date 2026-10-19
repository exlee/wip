-- | Command line client: talks to a running wip over its MCP endpoint.
module Wip.Cli
  ( usage
  , commands
  , Parsed (..)
  , parseArgs
  , run
  ) where

import Control.Exception (bracket, try)
import Control.Monad.Trans.Except
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.List (intercalate, isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe, isNothing)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Foreign.C.Error (Errno (..), eCONNREFUSED)
import GHC.IO.Exception (IOException (..))
import System.IO (hClose, hFlush)
import Text.Printf (printf)
import Text.Read (readMaybe)
import qualified Wip.Color as Color
import Wip.Json
import Wip.Socket (connectLoopback, socketHandle)

usage :: String
usage =
  "usage:\n\
  \  wip add [--port N] [--url=URL] [--id=ID] [--color=NAME|HEX] <name>   prints the item id\n\
  \  wip done [--port N] <id>\n\
  \  wip color [--port N] <id> <NAME|HEX|none>\n\
  \  wip delete [--port N] <id>\n\
  \  wip list [--port N]"

commands :: [String]
commands = ["add", "done", "color", "delete", "list"]

post :: Int -> String -> IO B.ByteString
post port body = do
  fd <- connectLoopback port
  bracket (socketHandle fd) hClose $ \h -> do
    let bytes = TE.encodeUtf8 (T.pack body)
    B8.hPut h . B8.pack $
      "POST /mcp HTTP/1.1\r\nHost: 127.0.0.1\r\nContent-Type: application/json\r\n\
      \Accept: application/json, text/event-stream\r\nContent-Length: "
        ++ show (B.length bytes) ++ "\r\n\r\n"
    B.hPut h bytes
    hFlush h
    B.hGetContents h

bodyOfResponse :: B.ByteString -> Maybe B.ByteString
bodyOfResponse resp =
  let (_, rest) = B.breakSubstring (B8.pack "\r\n\r\n") resp
   in if B.null rest then Nothing else Just (B.drop 4 rest)

-- | Calls an MCP tool; Right carries the tool's text output.
callTool :: Int -> String -> [(String, Json)] -> IO (Either String String)
callTool port name args = do
  let req =
        Obj
          [ ("jsonrpc", Str "2.0")
          , ("id", Num 1)
          , ("method", Str "tools/call")
          , ("params", Obj [("name", Str name), ("arguments", Obj args)])
          ]
  r <- try (post port (render req)) :: IO (Either IOException B.ByteString)
  pure $ case r of
    Left e
      | ioe_errno e == let Errno c = eCONNREFUSED in Just c ->
          Left (printf "cannot connect to 127.0.0.1:%d; is wip running?" port)
      | otherwise -> Left (ioe_description e)
    Right resp -> case bodyOfResponse resp of
      Nothing -> Left "bad response"
      Just body -> case parse (T.unpack (TE.decodeUtf8Lenient body)) of
        Left e -> Left ("bad response: " ++ e)
        Right reply -> case member "result" reply of
          Nothing -> Left (fromMaybe "error" (toStringOpt (member "error" reply >>= member "message")))
          Just result ->
            let text = case member "content" result of
                  Just (Arr (c : _)) -> fromMaybe "" (toStringOpt (member "text" c))
                  _ -> ""
             in if member "isError" result == Just (Bool True) then Left text else Right text

data Parsed = Parsed {port :: Int, url :: Maybe String, itemId :: Maybe String, color :: Maybe String, rest :: [String]}
  deriving (Eq, Show)

parseArgs :: Int -> [String] -> Either String Parsed
parseArgs defaultPort = go (Parsed defaultPort Nothing Nothing Nothing [])
 where
  go acc args = case args of
    [] -> Right acc {rest = reverse (rest acc)}
    [o] | o `elem` ["--url", "--port", "--id", "--color"] -> Left "missing value for option"
    "--color" : v : tl -> go acc {color = Just v} tl
    a : tl | Just v <- stripPrefix "--color=" a -> go acc {color = Just v} tl
    "--url" : v : tl -> go acc {url = Just v} tl
    "--id" : v : tl -> go acc {itemId = Just v} tl
    a : tl | Just v <- stripPrefix "--id=" a -> go acc {itemId = Just v} tl
    "--port" : v : tl -> setPort acc v tl
    a : tl | Just v <- stripPrefix "--url=" a -> go acc {url = Just v} tl
    a : tl | Just v <- stripPrefix "--port=" a -> setPort acc v tl
    a : _ | "--" `isPrefixOf` a -> Left ("unknown option " ++ a)
    a : tl -> go acc {rest = a : rest acc} tl
  setPort acc v tl = maybe (Left ("bad port " ++ v)) (\p -> go acc {port = p} tl) (readMaybe v)

idArg :: [String] -> Either String String
idArg [s] = Right s
idArg _ = Left "expected exactly one item id"

formatItems :: String -> String
formatItems text = case parse text of
  Right (Arr is) -> concatMap line is
  _ -> text ++ "\n"
 where
  line i =
    let s k = fromMaybe "" (toStringOpt (member k i))
     in intercalate "\t" [s "id", s "status", s "title", s "url"] ++ "\n"

-- | Runs @cmd@ with @args@; Right carries what to print on stdout.
run :: Int -> String -> [String] -> IO (Either String String)
run defaultPort cmd args = runExceptT $ do
  a <- except (parseArgs defaultPort args)
  let call name xs = ExceptT (callTool (port a) name xs)
  case cmd of
    "add" -> case rest a of
      [] -> throwE "add needs a name"
      _ | Just c <- color a, isNothing (Color.parse c) ->
            throwE ("bad color " ++ c ++ "; use a CSS colour name or hex like #f80 or #ff8800")
      ws -> do
        let opt k = maybe [] (\v -> [(k, Str v)])
        text <- call "add_item" ((("title", Str (unwords ws)) : opt "url" (url a)) ++ opt "id" (itemId a) ++ opt "color" (color a))
        case either (const Nothing) (toStringOpt . member "id") (parse text) of
          Just i -> pure (i ++ "\n")
          Nothing -> throwE ("unexpected reply: " ++ text)
    "done" -> do
      i <- except (idArg (rest a))
      "" <$ call "set_status" [("id", Str i), ("status", Str "done")]
    "color" -> case rest a of
      [i, c] -> "" <$ call "set_color" [("id", Str i), ("color", Str c)]
      _ -> throwE "color needs an item id and a colour"
    "delete" -> do
      i <- except (idArg (rest a))
      "" <$ call "delete_item" [("id", Str i)]
    "list" -> formatItems <$> call "list_items" []
    c -> throwE ("unknown command " ++ c ++ "\n" ++ usage)
