module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forM, unless)
import Data.Array ((!))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Either (isLeft, isRight)
import Data.IORef
import Data.List (isPrefixOf)
import Data.Maybe (fromJust, isJust, isNothing, mapMaybe)
import System.Directory (doesFileExist, getTemporaryDirectory, removeDirectoryRecursive)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hClose, hFlush)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Process (getProcessID)
import qualified Wip.Cli as Cli
import qualified Wip.Color as Color
import Wip.DeckLogic (Action (..), Pages (..), Slot (..))
import qualified Wip.DeckLogic as DL
import qualified Wip.Http as Http
import qualified Wip.Hyphen as Hyphen
import Wip.Json (Json (..))
import qualified Wip.Json as Json
import Wip.Layout (Hit (..))
import qualified Wip.Layout as Layout
import qualified Wip.Mcp as Mcp
import qualified Wip.Persist as Persist
import Wip.Socket (closeSocket, connectLoopback, listenLoopback, socketHandle)
import Wip.Store (Item (..), Status (..), Store)
import qualified Wip.Store as Store

failures :: IORef Int
failures = unsafePerformIO (newIORef 0)
{-# NOINLINE failures #-}

check :: String -> Bool -> IO ()
check name cond = unless cond $ do
  modifyIORef' failures (+ 1)
  putStrLn ("FAIL " ++ name)

rpc :: Store -> String -> Json -> IO (Maybe Json)
rpc store meth params = do
  let req = Obj [("jsonrpc", Str "2.0"), ("id", Num 1), ("method", Str meth), ("params", params)]
  r <- Mcp.handle store (Json.render req)
  pure (r >>= either (const Nothing) Just . Json.parse)

call :: Store -> String -> [(String, Json)] -> IO (Maybe Json)
call store name args = rpc store "tools/call" (Obj [("name", Str name), ("arguments", Obj args)])

resultOf :: Maybe Json -> Maybe Json
resultOf r = r >>= Json.member "result"

isError :: Maybe Json -> Bool
isError r = (resultOf r >>= Json.member "isError") == Just (Bool True)

resultText :: Maybe Json -> String
resultText r = case resultOf r >>= Json.member "content" of
  Just (Arr (c : _)) -> fromJust (Json.toStringOpt (Json.member "text" c))
  _ -> ""

parsed :: String -> Json
parsed = either error id . Json.parse

testJson :: IO ()
testJson = do
  let v = Obj [("s", Str "a\"b\\c\n\001"), ("n", Num 42), ("f", Num 1.5), ("l", Arr [Null, Bool False])]
  check "json roundtrip" (Json.parse (Json.render v) == Right v)
  check "json raw utf8" (Json.parse "\"é😀\"" == Right (Str "é😀"))
  check "json unicode escapes" (Json.parse "\"\\u00e9\\ud83d\\ude00\"" == Right (Str "é😀"))
  check "json rejects trailing" (isLeft (Json.parse "{} x"))
  check "json rejects unterminated" (isLeft (Json.parse "{\"a\": \"b"))
  check "json numbers" (Json.parse "[-1.5e2, 0.25, 7]" == Right (Arr [Num (-150), Num 0.25, Num 7]))

testStore :: IO ()
testStore = do
  s <- Store.create
  v0 <- Store.version s
  a <- Store.addNew s "a" "http://a" InProgress
  b <- Store.addNew s "b" "http://b" InProgress
  check "store ids unique" (itemId a /= itemId b)
  titles <- map itemTitle <$> Store.items s
  check "store keeps order" (titles == ["a", "b"])
  v1 <- Store.version s
  check "store version bumps" (v1 > v0)
  st <- Store.setStatus s (itemId a) Done
  check "store set_status" (fmap itemStatus st == Just Done)
  missing <- Store.setStatus s "nope" Done
  check "store set_status missing" (isNothing missing)
  deleted <- Store.delete s (itemId a)
  n <- length <$> Store.items s
  check "store delete" (deleted && n == 1)
  again <- Store.delete s (itemId a)
  check "store delete missing" (not again)
  c <- Store.add s (Just "2") Nothing "custom" "" InProgress
  d <- Store.addNew s "auto" "" InProgress
  found <- Store.find s "2"
  check "auto id skips taken custom id" (itemId c == "2" && itemId d /= "2" && itemId d /= itemId b && isJust found)

testMcp :: IO ()
testMcp = do
  s <- Store.create
  initR <- rpc s "initialize" (Obj [("protocolVersion", Str "2025-03-26")])
  check "initialize echoes version" (Json.toStringOpt (resultOf initR >>= Json.member "protocolVersion") == Just "2025-03-26")
  note <- Mcp.handle s "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}"
  check "notification has no response" (isNothing note)
  listed <- rpc s "tools/list" (Obj [])
  case resultOf listed >>= Json.member "tools" of
    Just (Arr ts) -> do
      let names = mapMaybe (Json.toStringOpt . Json.member "name") ts
      mapM_ (\n -> check ("tools/list has " ++ n) (n `elem` names)) ["list_items", "add_item", "delete_item", "set_status", "set_color"]
    _ -> check "tools/list shape" False
  added <- call s "add_item" [("title", Str "Review PR"), ("url", Str "https://example.com/pr/1")]
  check "add_item ok" (not (isError added))
  let item = parsed (resultText added)
      i = fromJust (Json.toStringOpt (Json.member "id" item))
  check "add_item defaults to in_progress" (Json.member "status" item == Just (Str "in_progress"))
  stored <- any ((== i) . itemId) <$> Store.items s
  check "add_item stored" stored
  do
    s2 <- Store.create
    r <- call s2 "add_item" [("title", Str "x")]
    is <- Store.items s2
    check "add_item without url" (not (isError r) && map itemUrl is == [""])
  do
    s2 <- Store.create
    r <- call s2 "add_item" [("title", Str "x"), ("color", Str "red")]
    check "add_item color reported as hex" (Json.toStringOpt (Json.member "color" (parsed (resultText r))) == Just "#ff0000")
  let errs label name args = call s name args >>= check label . isError
  errs "add_item invalid color" "add_item" [("title", Str "x"), ("color", Str "#xyz")]
  errs "add_item non-string color" "add_item" [("title", Str "x"), ("color", Num 1)]
  errs "add_item missing title" "add_item" [("url", Str "u")]
  errs "add_item non-string url" "add_item" [("title", Str "x"), ("url", Num 1)]
  errs "add_item bad status" "add_item" [("title", Str "x"), ("url", Str "u"), ("status", Str "later")]
  set <- call s "set_status" [("id", Str i), ("status", Str "done")]
  check "set_status ok" (not (isError set))
  firstStatus <- map itemStatus . take 1 <$> Store.items s
  check "set_status applied" (firstStatus == [Done])
  errs "set_status unknown id" "set_status" [("id", Num 999), ("status", Str "done")]
  errs "set_status requires status" "set_status" [("id", Str i)]
  listR <- call s "list_items" []
  case Json.parse (resultText listR) of
    Right (Arr [_]) -> pure ()
    _ -> check "list_items returns one item" False
  del <- call s "delete_item" [("id", Str i)]
  check "delete_item ok" (not (isError del))
  remaining <- Store.items s
  check "delete_item removed" (null remaining)
  errs "delete_item twice errors" "delete_item" [("id", Str i)]
  unknownTool <- call s "nope" []
  check "unknown tool errors" (isError unknownTool)
  unknownMethod <- rpc s "nope/nope" (Obj [])
  check "unknown method" (isJust (unknownMethod >>= Json.member "error"))
  bad <- Mcp.handle s "{"
  check "parse error" (isJust (bad >>= either (const Nothing) Just . Json.parse >>= Json.member "error"))

httpPost :: Int -> Maybe String -> String -> IO String
httpPost port origin body = do
  fd <- connectLoopback port
  bracket (socketHandle fd) hClose $ \h -> do
    B8.hPut h . B8.pack $
      "POST /mcp HTTP/1.1\r\nHost: localhost\r\nContent-Type: application/json\r\n"
        ++ maybe "" (\o -> "Origin: " ++ o ++ "\r\n") origin
        ++ "Content-Length: " ++ show (length body) ++ "\r\n\r\n" ++ body
    hFlush h
    B8.unpack <$> B.hGetContents h

testHttp :: IO ()
testHttp = do
  s <- Store.create
  port <- Http.start s 0
  resp <- httpPost port Nothing "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"tools/call\",\"params\":{\"name\":\"add_item\",\"arguments\":{\"title\":\"t\",\"url\":\"u\"}}}"
  check "http 200" ("HTTP/1.1 200" `isPrefixOf` resp)
  n <- length <$> Store.items s
  check "http adds item" (n == 1)
  note <- httpPost port Nothing "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}"
  check "http notification 202" ("HTTP/1.1 202" `isPrefixOf` note)
  foreign_ <- httpPost port (Just "https://evil.example") "{}"
  check "http foreign origin 403" ("HTTP/1.1 403" `isPrefixOf` foreign_)
  check "http header lookup is case-insensitive" (Http.header "content-length" ["Content-Length: 12"] == Just "12")

testLayout :: IO ()
testLayout = do
  let hit = Layout.hit 2
  check "layout close button" (hit (Layout.width - 1) 1 == Close)
  check "layout header" (hit 10 1 == Header)
  check "layout first row" (hit 10 (Layout.rowY 0 + 1) == Row 0)
  check "layout second row" (hit 10 (Layout.rowY 1 + 1) == Row 1)
  check "layout past rows" (hit 10 (Layout.rowY 2 + 1) == NoHit)
  check "layout grows with rows" (Layout.height 3 > Layout.height 2)
  let long = replicate 200 'a'
  check "fit_text cuts" (fromIntegral (length (Layout.fitText 12 long 100)) * 12 <= (100 :: Double))
  check "marquee holds at start" (Layout.marqueeOffset 60 1 == 0)
  check "marquee scrolls" (let o = Layout.marqueeOffset 60 3 in o > 0 && o < 60)
  check "marquee stops at end" (Layout.marqueeOffset 60 4.5 == 60)
  check "marquee restarts" (Layout.marqueeOffset 60 5.1 == 0)
  check "marquee idle when it fits" (Layout.marqueeOffset (-5) 3 == 0)
  check "fit_text keeps short" (Layout.fitText 12 "ok" 100 == "ok")
  let accented = replicate 50 'é'
      cut = Layout.fitText 10 accented 100
  check "fit_text counts characters" (length cut == 10 && take 8 cut == take 8 accented)

testDeckLogic :: IO ()
testDeckLogic = do
  s <- Store.create
  a <- Store.addNew s "a" "u" InProgress
  b <- Store.addNew s "b" "u" Done
  ordered <- DL.byActivity <$> Store.items s
  check "newest activity first" (map itemId ordered == [itemId b, itemId a])
  _ <- Store.setStatus s (itemId a) Done
  reordered <- DL.byActivity <$> Store.items s
  check "status change moves item first" (map itemId (take 1 reordered) == [itemId a])
  check "latest activity grows" (DL.latestActivity reordered > DL.latestActivity ordered)
  let itemAt arr k = case arr ! k of ItemSlot i -> Just (itemId i); _ -> Nothing
      sl = DL.slots 3 True 0 ordered
  check "slots fill in order" (itemAt sl 0 == Just (itemId b) && itemAt sl 1 == Just (itemId a) && sl ! 2 == Empty)
  many <- forM [0 .. 6 :: Int] $ \n -> Store.addNew s (show n) "" InProgress
  let pg = DL.pages 3 True 7
  check "touch paging uses every key" (perPage pg == 3 && count pg == 3 && navKey pg == Nothing)
  let lastPage = DL.slots 3 True 2 many
  check "last page holds the remainder" (itemAt lastPage 0 == Just (itemId (many !! 6)) && lastPage ! 1 == Empty)
  let pn = DL.pages 3 False 7
  check "nav key when no touch buttons" (perPage pn == 2 && count pn == 4 && navKey pn == Just 2)
  check "nav key slot" (DL.slots 3 False 1 many ! 2 == Nav 1 4)
  check "no nav key when items fit" (navKey (DL.pages 3 False 3) == Nothing)
  check "page clamps" (itemAt (DL.slots 3 True 99 many) 0 == Just (itemId (many !! 6)))
  check "forward and back stop at ends" (DL.forward pg 2 == 2 && DL.back pg 0 == 0 && DL.forward pg 0 == 1 && DL.back pg 2 == 1)
  check "nav key cycles" (DL.cycle pn 3 == 0 && DL.cycle pn 0 == 1)
  (w, d, t) <- DL.counts <$> Store.items s
  check "counts" (w + d == t && d == 2)
  let p0 = DL.down 0 (Just (itemId a)) 0 (DL.newPressWith 0.5)
      (short, p1) = DL.up 0 0.2 p0
  check "short press opens" (short == Just (Open (itemId a)))
  let p2 = DL.down 1 (Just (itemId b)) 1 p1
      (early, p3) = DL.tick 1.3 p2
      (long, p4) = DL.tick 1.6 p3
      (twice, p5) = DL.tick 2 p4
      (release, p6) = DL.up 1 2.1 p5
  check "no action before threshold" (null early)
  check "long press deletes while held" (long == [Delete (itemId b)])
  check "long press fires once" (null twice)
  check "release after long press is quiet" (isNothing release)
  let p7 = DL.down 2 Nothing 3 p6
      (emptyUp, p8) = DL.up 2 3.1 p7
  check "empty key does nothing" (isNothing emptyUp && null (fst (DL.tick 9 p8)))
  check "up without down" (isNothing (fst (DL.up 5 1 p8)))

testCli :: IO ()
testCli = do
  s <- Store.create
  port <- Http.start s 0
  let run = Cli.run port
      findWith f i = fmap f <$> Store.find s i
      trimmed = takeWhile (/= '\n')
  run "add" ["--url=https://example.com", "Ship", "it"] >>= \case
    Left e -> check ("cli add: " ++ e) False
    Right out | null (trimmed out) -> check "cli add prints id" False
    Right out -> do
      let i = trimmed out
      title <- findWith itemTitle i
      check "cli add stores joined name" (title == Just "Ship it")
      doneR <- run "done" [i]
      check "cli done" (doneR == Right "")
      st <- findWith itemStatus i
      check "cli done applied" (st == Just Done)
      run "list" [] >>= \case
        Right listed -> check "cli list shows item" (length (lines listed) == 1 && length (words listed) >= 3)
        Left _ -> check "cli list" False
      delR <- run "delete" [i]
      check "cli delete" (delR == Right "")
      gone <- Store.find s i
      check "cli delete applied" (isNothing gone)
      again <- run "delete" [i]
      check "cli delete missing errors" (isLeft again)
  urlForm <- run "add" ["--url", "u", "x"]
  check "cli add --url VALUE form" (isRight urlForm)
  run "add" ["No", "link"] >>= \case
    Right out -> findWith itemUrl (trimmed out) >>= check "cli add without url" . (== Just "")
    Left e -> check ("cli add without url: " ++ e) False
  noName <- run "add" ["--url=u"]
  check "cli add needs name" (isLeft noName)
  unknownId <- run "done" ["abc"]
  check "cli unknown id" (isLeft unknownId)
  custom <- run "add" ["--id=pr-42", "Review", "PR"]
  check "cli add custom id" (custom == Right "pr-42\n")
  doneCustom <- run "done" ["pr-42"]
  check "cli done custom id" (doneCustom == Right "")
  n <- length <$> Store.items s
  readd <- run "add" ["--id", "pr-42", "--url=https://x", "Review", "again"]
  n' <- length <$> Store.items s
  updated <- Store.find s "pr-42"
  check
    "cli re-add updates in place"
    (readd == Right "pr-42\n" && n' == n && fmap (\i -> (itemTitle i, itemStatus i)) updated == Just ("Review again", InProgress))
  delCustom <- run "delete" ["pr-42"]
  goneCustom <- Store.find s "pr-42"
  check "cli delete custom id" (delCustom == Right "" && isNothing goneCustom)
  spaced <- run "add" ["--id=a b", "x"]
  check "cli rejects id with space" (isLeft spaced)
  emptyId <- run "add" ["--id=", "x"]
  check "cli rejects empty id" (isLeft emptyId)
  let colorOf out = Store.find s (trimmed out) >>= pure . (>>= itemColor)
  run "add" ["--color=steelblue", "Tinted"] >>= \case
    Right out -> colorOf out >>= check "cli add named color" . (== Color.parse "steelblue")
    Left e -> check ("cli add named color: " ++ e) False
  run "add" ["--color", "#0f0", "Hex"] >>= \case
    Right out -> colorOf out >>= check "cli add hex color" . (== Just (0, 255, 0))
    Left e -> check ("cli add hex color: " ++ e) False
  run "add" ["Plain"] >>= \case
    Right out -> colorOf out >>= check "cli add without color" . isNothing
    Left e -> check ("cli add without color: " ++ e) False
  badColor <- run "add" ["--color=nope", "x"]
  check "cli rejects bad color" (isLeft badColor)
  noValue <- run "add" ["x", "--color"]
  check "cli color needs value" (isLeft noValue)
  run "add" ["Recolor"] >>= \case
    Right out -> do
      let i = trimmed out
          color = (>>= itemColor) <$> Store.find s i
          activity = fmap itemActivity <$> Store.find s i
      before <- activity
      r1 <- run "color" [i, "navy"]
      c1 <- color
      check "cli color sets" (r1 == Right "" && c1 == Color.parse "navy")
      after <- activity
      check "cli color keeps activity" (after == before)
      r2 <- run "color" [i, "#123"]
      c2 <- color
      check "cli color hex" (r2 == Right "" && c2 == Just (0x11, 0x22, 0x33))
      r3 <- run "color" [i, "none"]
      c3 <- color
      check "cli color none clears" (r3 == Right "" && isNothing c3)
      r4 <- run "color" [i, "nope"]
      check "cli color rejects bad value" (isLeft r4)
      r5 <- run "color" [i]
      check "cli color needs two args" (isLeft r5)
    Left e -> check ("cli color: " ++ e) False
  unknownColor <- run "color" ["missing", "red"]
  check "cli color unknown id" (isLeft unknownColor)
  unknownOpt <- run "list" ["--nope"]
  check "cli unknown option" (isLeft unknownOpt)
  closed <- do
    (sock, p) <- listenLoopback 0 1
    closeSocket sock
    pure p
  unreachable <- Cli.run closed "list" []
  check "cli reports unreachable server" (isLeft unreachable)

testPersist :: IO ()
testPersist = do
  tmp <- getTemporaryDirectory
  pid <- getProcessID
  let dir = tmp </> ("wip-test-" ++ show pid)
      path = dir </> "nested" </> "session.json"
  a <- Store.create
  x <- Store.addNew a "x" "https://x" InProgress
  _ <- Store.add a (Just "pr-1") (Just (1, 2, 3)) "y \"quoted\"" "" InProgress
  _ <- Store.setStatus a (itemId x) Done
  Persist.save path a
  b <- Store.create
  loaded <- Persist.load path b
  check "persist load ok" (loaded == Right True)
  ia <- Store.items a
  ib <- Store.items b
  check "persist items round trip" (ia == ib)
  nextA <- Store.addNew a "n" "" InProgress
  nextB <- Store.addNew b "n" "" InProgress
  check "persist keeps id counter and activity" (itemId nextA == itemId nextB && itemActivity nextA == itemActivity nextB)
  none <- Store.create >>= Persist.load (path ++ ".none")
  check "persist missing file" (none == Right False)
  writeFile path "{ not json"
  c <- Store.create
  broken <- Persist.load path c
  ic <- Store.items c
  check "persist broken file is an error" (isLeft broken && null ic)
  bad <- doesFileExist (path ++ ".bad")
  still <- doesFileExist path
  check "persist moves broken file aside" (bad && not still)
  d <- Store.create
  Persist.autosaveEvery 0.02 path d
  _ <- Store.addNew d "auto" "" InProgress
  threadDelay 200000
  e <- Store.create
  autoLoaded <- Persist.load path e
  ie <- Store.items e
  check "autosave writes changes" (autoLoaded == Right True && length ie == 1)
  removeDirectoryRecursive dir

testColor :: IO ()
testColor = do
  check "color named" (Color.parse "Tomato" == Just (0xFF, 0x63, 0x47))
  check "color hex long" (Color.parse "#1a2B3c" == Just (0x1A, 0x2B, 0x3C))
  check "color hex short" (Color.parse "#f80" == Just (0xFF, 0x88, 0x00))
  check "color hex without hash" (Color.parse "ff8800" == Color.parse "#ff8800")
  check "color rejects junk" (all (isNothing . Color.parse) ["", "#", "#ff88", "#gg0000", "notacolor", "#ff88001"])
  check "color hex round trip" (fmap Color.toHex (Color.parse "#0a0B0c") == Just "#0a0b0c")
  check "color contrast" (Color.isLight (255, 255, 255) && not (Color.isLight (0, 0, 128)))
  check "color lighten moves to white" (Color.lighten (0, 0, 0) > (0, 0, 0) && Color.lighten (255, 255, 255) == (255, 255, 255))

testHyphen :: IO ()
testHyphen = do
  let pieces mx = Hyphen.split ((<= mx) . length)
      breaksAt w = map (`take` w) (Hyphen.breaks w)
      word = "Internationalization"
  check "hyphen syllables" ("In" `elem` breaksAt word && "Inter" `elem` breaksAt word)
  check "hyphen keeps two letters each side" (all (\p -> p >= 2 && length word - p >= 2) (Hyphen.breaks word))
  check "hyphen digraph stays together" ("pro" `elem` breaksAt "prophet")
  check "hyphen short word has no breaks" (null (Hyphen.breaks "cat"))
  let p = pieces 8 word
  check "hyphen pieces rebuild word" (concat p == word)
  check "hyphen pieces fit with dash" (all (\x -> length x + 1 <= 8) (init p))
  check "hyphen takes longest syllable prefix" (take 1 p == ["Interna"])
  check "hyphen fitting word untouched" (pieces 20 "short" == ["short"])
  let hard = pieces 4 "xkcdqwrtz"
  check "hyphen falls back to char breaks" (concat hard == "xkcdqwrtz" && length hard > 1)
  let u = pieces 3 "ééééé"
  check "hyphen splits non-ascii by character" (concat u == "ééééé" && all ((<= 3) . length) u)

main :: IO ()
main = do
  testJson
  testStore
  testMcp
  testHttp
  testLayout
  testDeckLogic
  testCli
  testPersist
  testHyphen
  testColor
  n <- readIORef failures
  if n > 0
    then putStrLn (show n ++ " failure(s)") >> exitFailure
    else putStrLn "all tests passed"
