-- | MCP (JSON-RPC 2.0) request handling for the work item store.
module Wip.Mcp (protocolVersion, itemJson, handle) where

import Data.Char (isSpace, ord, toLower)
import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe, isJust)
import Text.Printf (printf)
import qualified Wip.Color as Color
import Wip.Json
import Wip.Store (Item (..), Status (..), Store)
import qualified Wip.Store as Store

protocolVersion :: String
protocolVersion = "2025-06-18"

itemJson :: Item -> Json
itemJson i =
  Obj
    [ ("id", Str (itemId i))
    , ("title", Str (itemTitle i))
    , ("url", Str (itemUrl i))
    , ("status", Str (Store.statusToString (itemStatus i)))
    , ("color", maybe Null (Str . Color.toHex) (itemColor i))
    ]

statusSchema :: Json
statusSchema = Obj [("type", Str "string"), ("enum", Arr [Str "in_progress", Str "done"])]

idSchema :: Json
idSchema = Obj [("type", Str "string"), ("description", Str "Item id")]

-- | Ids are strings; plain integers are accepted for convenience.
idArg :: Json -> Maybe String
idArg args = case member "id" args of
  Just (Str s) -> Just s
  Just n@(Num _) -> show <$> toIntOpt (Just n)
  _ -> Nothing

validId :: String -> Bool
validId s = not (null s) && length s <= 64 && all (\c -> ord c > 32 && ord c /= 127) s

tool :: String -> String -> [(String, Json)] -> [String] -> Json
tool name description properties required =
  Obj
    [ ("name", Str name)
    , ("description", Str description)
    , ( "inputSchema"
      , Obj
          [ ("type", Str "object")
          , ("properties", Obj properties)
          , ("required", Arr (map Str required))
          ]
      )
    ]

tools :: [Json]
tools =
  [ tool "list_items" "List all work items with id, title, url and status." [] []
  , tool
      "add_item"
      "Add a work item and return it. Status defaults to in_progress."
      [ ("title", Obj [("type", Str "string")])
      , ("url", Obj [("type", Str "string"), ("description", Str "Optional; opened when the item is clicked")])
      , ("status", statusSchema)
      , ("id", Obj [("type", Str "string"), ("description", Str "Optional custom id, no whitespace; an existing id updates that item")])
      , ("color", Obj [("type", Str "string"), ("description", Str "Optional background: CSS colour name or hex (#rgb, #rrggbb)")])
      ]
      ["title"]
  , tool "delete_item" "Delete a work item by id." [("id", idSchema)] ["id"]
  , tool "set_status" "Change the status of a work item." [("id", idSchema), ("status", statusSchema)] ["id", "status"]
  , tool
      "set_color"
      "Set or clear the background colour of a work item."
      [ ("id", idSchema)
      , ("color", Obj [("type", Str "string"), ("description", Str "CSS colour name or hex (#rgb, #rrggbb); \"none\" or null clears it")])
      ]
      ["id", "color"]
  ]

textResult :: Bool -> Json -> Json
textResult isError v =
  let text = case v of Str s -> s; _ -> render v
   in Obj [("content", Arr [Obj [("type", Str "text"), ("text", Str text)]]), ("isError", Bool isError)]

ok :: Json -> Json
ok = textResult False

toolError :: String -> Json
toolError = textResult True . Str

parseStatus :: Json -> Status -> Either String Status
parseStatus args def = case member "status" args of
  Nothing -> Right def
  Just (Str s) -> maybe (Left (printf "invalid status %s, expected in_progress or done" (show s))) Right (Store.statusOfString s)
  Just _ -> Left "status must be a string"

parseColor :: Json -> Either String (Maybe Color.Color)
parseColor args = case member "color" args of
  Nothing -> Right Nothing
  Just Null -> Right Nothing
  Just (Str s)
    | map toLower (dropWhileEnd isSpace (dropWhile isSpace s)) == "none" -> Right Nothing
    | otherwise ->
        maybe
          (Left (printf "invalid color %s, expected a CSS colour name or hex like #f80 or #ff8800" (show s)))
          (Right . Just)
          (Color.parse s)
  Just _ -> Left "color must be a string"

noItem :: String -> Json
noItem i = toolError ("no item with id " ++ i)

callTool :: Store -> String -> Json -> IO Json
callTool store name args = case name of
  "list_items" -> ok . Arr . map itemJson <$> Store.items store
  "add_item" -> case (toStringOpt (member "title" args), member "url" args) of
    (Nothing, _) -> pure (toolError "title is required and must be a string")
    (Just _, Just u) | toStringOpt (Just u) == Nothing -> pure (toolError "url must be a string")
    (Just _, _)
      | isJust (member "id" args) && not (maybe False validId (idArg args)) ->
          pure (toolError "id must be a non-empty string without whitespace (max 64 chars)")
    (Just title, url) ->
      case (parseStatus args InProgress, parseColor args) of
        (Right status, Right color) ->
          ok . itemJson <$> Store.add store (idArg args) color title (fromMaybe "" (toStringOpt url)) status
        (Left e, _) -> pure (toolError e)
        (_, Left e) -> pure (toolError e)
  "delete_item" -> case idArg args of
    Nothing -> pure (toolError "id must be a string")
    Just i -> do
      removed <- Store.delete store i
      pure (if removed then ok (Str ("deleted item " ++ i)) else noItem i)
  "set_status" -> case (idArg args, member "status" args) of
    (Nothing, _) -> pure (toolError "id must be a string")
    (_, Nothing) -> pure (toolError "status is required")
    (Just i, Just _) -> case parseStatus args InProgress of
      Left e -> pure (toolError e)
      Right status -> maybe (noItem i) (ok . itemJson) <$> Store.setStatus store i status
  "set_color" -> case (idArg args, member "color" args) of
    (Nothing, _) -> pure (toolError "id must be a string")
    (_, Nothing) -> pure (toolError "color is required")
    (Just i, Just _) -> case parseColor args of
      Left e -> pure (toolError e)
      Right color -> maybe (noItem i) (ok . itemJson) <$> Store.setColor store i color
  _ -> pure (toolError (printf "unknown tool %s" (show name)))

response :: Json -> Json -> Json
response i result = Obj [("jsonrpc", Str "2.0"), ("id", i), ("result", result)]

errorResponse :: Json -> Int -> String -> Json
errorResponse i code message =
  Obj
    [ ("jsonrpc", Str "2.0")
    , ("id", i)
    , ("error", Obj [("code", Num (fromIntegral code)), ("message", Str message)])
    ]

dispatch :: Store -> Json -> IO (Maybe Json)
dispatch store msg =
  case (member "id" msg, toStringOpt (member "method" msg)) of
    (Nothing, _) -> pure Nothing -- notification: nothing to send back
    (Just i, Nothing) -> pure (Just (errorResponse i (-32600) "invalid request"))
    (Just i, Just m) -> Just <$> case m of
      "initialize" ->
        let v = fromMaybe protocolVersion (toStringOpt (member "protocolVersion" params))
         in pure . response i $
              Obj
                [ ("protocolVersion", Str v)
                , ("capabilities", Obj [("tools", Obj [])])
                , ("serverInfo", Obj [("name", Str "wip"), ("version", Str "0.1.0")])
                ]
      "ping" -> pure (response i (Obj []))
      "tools/list" -> pure (response i (Obj [("tools", Arr tools)]))
      "tools/call" -> case toStringOpt (member "name" params) of
        Nothing -> pure (errorResponse i (-32602) "missing tool name")
        Just name -> response i <$> callTool store name (fromMaybe (Obj []) (member "arguments" params))
      _ -> pure (errorResponse i (-32601) ("method not found: " ++ m))
 where
  params = fromMaybe (Obj []) (member "params" msg)

-- | Returns the response body, or Nothing when the request needs no response.
handle :: Store -> String -> IO (Maybe String)
handle store body = case parse body of
  Left e -> pure (Just (render (errorResponse Null (-32700) ("parse error: " ++ e))))
  Right msg -> fmap render <$> dispatch store msg
