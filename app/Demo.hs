-- | Demo: the floating window seeded with sample items. One item flips status
-- every few seconds to show live updates. The MCP server runs as usual.
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (finally)
import Control.Monad (forM_, forever, void, when)
import Data.IORef
import qualified Args
import qualified Deck
import qualified Gui
import System.Environment (getArgs)
import System.IO
import Wip.AtExit (runExitHooks)
import qualified Wip.Http as Http
import Wip.Store (Item (..), Status (..))
import qualified Wip.Store as Store

samples :: [(String, String, Status)]
samples =
  [ ("Review Haskell SDL3 bindings", "https://github.com/libsdl-org/SDL", InProgress)
  , ("Write MCP server docs", "https://modelcontextprotocol.io", Done)
  , ("Fix flaky HTTP test", "https://haskell.org", InProgress)
  , ("A very long title that does not fit the window width at all", "https://example.com", InProgress)
  , ("Release 0.1.0", "https://hackage.haskell.org", Done)
  ]

main :: IO ()
main = flip finally runExitHooks $ do
  hSetBuffering stdout LineBuffering
  font <- newIORef Nothing
  port <- newIORef 7879
  flip' <- newIORef 3
  deck <- newIORef True
  getArgs
    >>= Args.parse
      [ ("--port", Args.SetInt port, "PORT  MCP HTTP port on 127.0.0.1 (default 7879)")
      , ("--font", Args.SetString (writeIORef font . Just), "PATH  TrueType/OpenType font file (default SF Mono, then Menlo)")
      , ("--flip", Args.SetFloat flip', "SECONDS  Status flip interval, 0 disables (default 3)")
      , ("--no-deck", Args.Clear deck, " Do not use a Stream Deck")
      ]
      "demo [--port PORT] [--font PATH] [--flip SECONDS] [--no-deck]"
  store <- Store.create
  forM_ samples $ \(title, url, status) -> Store.addNew store title url status
  interval <- readIORef flip'
  when (interval > 0) . void . forkIO . forever $ do
    threadDelay (round (interval * 1e6))
    Store.items store >>= \case
      first : _ -> do
        let next = if itemStatus first == Done then InProgress else Done
        void (Store.setStatus store (itemId first) next)
      [] -> pure ()
  bound <- readIORef port >>= Http.start store
  putStrLn ("MCP endpoint: http://127.0.0.1:" ++ show bound ++ "/mcp")
  fontPath <- readIORef font
  useDeck <- readIORef deck
  when useDeck (Deck.start store fontPath)
  Gui.runGui fontPath store bound
