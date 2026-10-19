-- | Floating always-on-top list of work items, controlled over MCP, mirrored on
-- a Stream Deck. With a command (add, done, color, delete, list) it acts as a
-- client of a running instance instead.
module Main (main) where

import Control.Concurrent (myThreadId, threadDelay, throwTo)
import Control.Exception (finally)
import Control.Monad (forM_, forever, when)
import Data.IORef
import qualified Args
import qualified Deck
import qualified Gui
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitWith)
import System.IO
import System.Posix.Signals (Handler (..), installHandler, sigINT, sigTERM)
import Wip.AtExit (runExitHooks)
import qualified Wip.Cli as Cli
import qualified Wip.Http as Http
import qualified Wip.Persist as Persist
import qualified Wip.Store as Store

defaultPort :: Int
defaultPort = 7878

serve :: [String] -> IO ()
serve args = do
  font <- newIORef Nothing
  port <- newIORef defaultPort
  noUi <- newIORef False
  deck <- newIORef True
  state <- newIORef Nothing
  persist <- newIORef True
  Args.parse
    [ ("--port", Args.SetInt port, "PORT  MCP HTTP port on 127.0.0.1 (default 7878)")
    , ("--font", Args.SetString (writeIORef font . Just), "PATH  TrueType/OpenType font file (default SF Mono, then Menlo)")
    , ("--no-ui", Args.Set noUi, " Do not show the floating window")
    , ("--headless", Args.Set noUi, " Same as --no-ui")
    , ("--no-deck", Args.Clear deck, " Do not use a Stream Deck")
    , ("--state", Args.SetString (writeIORef state . Just), "PATH  Session file (default ~/.local/state/wip/session-PORT.json)")
    , ("--no-state", Args.Clear persist, " Do not load or save the session")
    ]
    ("wip [--port PORT] [--font PATH] [--no-ui] [--no-deck] [--state PATH] [--no-state]\n" ++ Cli.usage)
    args
  store <- Store.create
  requested <- readIORef port
  doPersist <- readIORef persist
  when doPersist $ do
    path <- maybe (Persist.defaultPath requested) pure =<< readIORef state
    Persist.load path store >>= \case
      Right True -> Store.items store >>= \is -> putStrLn ("Session: " ++ path ++ " (" ++ show (length is) ++ " items)")
      Right False -> putStrLn ("Session: " ++ path ++ " (new)")
      Left e -> putStrLn ("Session: " ++ path ++ " unreadable (" ++ e ++ "), moved to .bad, starting empty")
    Persist.autosave path store
  bound <- Http.start store requested
  putStrLn ("MCP endpoint: http://127.0.0.1:" ++ show bound ++ "/mcp")
  fontPath <- readIORef font
  useDeck <- readIORef deck
  when useDeck (Deck.start store fontPath)
  headless <- readIORef noUi
  if headless
    then do
      -- Exiting runs the exit hooks, which blank the Stream Deck keys.
      mainThread <- myThreadId
      forM_ [sigINT, sigTERM] $ \s ->
        installHandler s (Catch (throwTo mainThread ExitSuccess)) Nothing
      forever (threadDelay 3600000000)
    else Gui.runGui fontPath store bound

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  args <- getArgs
  flip finally runExitHooks $ case args of
    cmd : rest | cmd `elem` Cli.commands ->
      Cli.run defaultPort cmd rest >>= \case
        Right out -> putStr out
        Left e -> do
          hPutStrLn stderr ("wip: " ++ e)
          exitWith (ExitFailure 1)
    _ -> serve args
