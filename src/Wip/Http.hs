-- | Tiny HTTP/1.1 server for the MCP Streamable HTTP transport (JSON responses only).
module Wip.Http (start, header, originAllowed) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, catch, finally, try)
import Control.Monad (forever, void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.IO
import System.IO.Error (isEOFError)
import Text.Read (readMaybe)
import qualified Wip.Mcp as Mcp
import Wip.Socket
import Wip.Store (Store)

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

readHeaders :: Handle -> IO [String]
readHeaders h = loop []
 where
  loop acc = do
    r <- try (B8.hGetLine h)
    case r of
      Left e | isEOFError e -> pure (reverse acc)
      Left e -> ioError e
      Right raw ->
        let line = trim (B8.unpack raw)
         in if null line then pure (reverse acc) else loop (line : acc)

-- | Value of header @name@ (case-insensitive) among raw header lines.
header :: String -> [String] -> Maybe String
header name headers =
  listToMaybe
    [ trim (drop 1 v)
    | h <- headers
    , let (k, v) = break (== ':') h
    , not (null v)
    , map toLower k == map toLower name
    ]

-- | Rejects browser requests from foreign origins (DNS rebinding protection).
originAllowed :: Maybe String -> Bool
originAllowed Nothing = True
originAllowed (Just o) =
  any (`isPrefixOf` o) ["http://localhost", "http://127.0.0.1", "https://localhost", "https://127.0.0.1"]

respond :: Handle -> String -> String -> IO ()
respond h status body = do
  let bytes = TE.encodeUtf8 (T.pack body)
  B8.hPut h $
    B8.pack
      ( "HTTP/1.1 " ++ status ++ "\r\nContent-Type: application/json\r\nContent-Length: "
          ++ show (B.length bytes) ++ "\r\nConnection: close\r\n\r\n"
      )
  B.hPut h bytes
  hFlush h

handleConnection :: Store -> Handle -> IO ()
handleConnection store h =
  readHeaders h >>= \case
    [] -> pure ()
    requestLine : headers -> do
      let (meth, path) = case words requestLine of
            m : p : _ -> (m, p)
            _ -> ("", "")
          mcpPath = path == "/mcp" || path == "/"
      if not (originAllowed (header "origin" headers))
        then respond h "403 Forbidden" ""
        else case meth of
          "POST" | mcpPath -> do
            let len = fromMaybe 0 (header "content-length" headers >>= readMaybe)
            raw <- B.hGet h len
            if B.length raw < len then ioError (userError "short body") else pure ()
            Mcp.handle store (T.unpack (TE.decodeUtf8Lenient raw)) >>= \case
              Just resp -> respond h "200 OK" resp
              Nothing -> respond h "202 Accepted" ""
          _ | meth `elem` ["GET", "DELETE"] && mcpPath -> respond h "405 Method Not Allowed" ""
          _ -> respond h "404 Not Found" ""

-- | Binds 127.0.0.1:port (0 picks a free port), serves in a background thread,
-- and returns the bound port.
start :: Store -> Int -> IO Int
start store port = do
  (sock, bound) <- listenLoopback port 16
  void . forkIO . forever $ do
    fd <- accept sock
    forkIO $ do
      h <- socketHandle fd
      (handleConnection store h `finally` hClose h) `catch` \(_ :: SomeException) -> pure ()
  pure bound
