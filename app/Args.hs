-- | Option parsing in the style of OCaml's Arg: each option updates an
-- 'IORef', positional arguments are ignored, @--help@ prints the usage.
module Args (Spec (..), parse) where

import Data.IORef
import Data.List (stripPrefix)
import System.Environment (getProgName)
import System.Exit (exitSuccess, exitWith, ExitCode (..))
import System.IO (hPutStr, stderr)
import Text.Read (readMaybe)

data Spec
  = SetInt (IORef Int)
  | SetFloat (IORef Double)
  | SetString (String -> IO ())
  | Set (IORef Bool)
  | Clear (IORef Bool)

takesValue :: Spec -> Bool
takesValue = \case
  Set _ -> False
  Clear _ -> False
  _ -> True

-- | Applies @specs@ (option, action, doc) to @args@. On an error prints the
-- message and usage to stderr and exits with code 2.
parse :: [(String, Spec, String)] -> String -> [String] -> IO ()
parse specs usage = go
 where
  help = usage ++ "\n" ++ concat ["  " ++ o ++ " " ++ d ++ "\n" | (o, _, d) <- specs] ++ "  -help  Display this list of options\n  --help  Display this list of options\n"
  bad msg = do
    prog <- getProgName
    hPutStr stderr (prog ++ ": " ++ msg ++ ".\n" ++ help)
    exitWith (ExitFailure 2)
  go = \case
    [] -> pure ()
    a : rest
      | a `elem` ["-help", "--help"] -> putStr help >> exitSuccess
      | otherwise -> case [(s, v) | (o, s, _) <- specs, v <- matches o a] of
          (s, Just v) : _ | takesValue s -> apply a s v >> go rest
          (s, Nothing) : _
            | takesValue s -> case rest of
                v : rest' -> apply a s v >> go rest'
                [] -> bad ("option '" ++ a ++ "' needs an argument")
            | otherwise -> apply a s "" >> go rest
          _ | take 1 a == "-" -> bad ("unknown option '" ++ a ++ "'")
          _ -> go rest
  -- "--opt" alone, or "--opt=value".
  matches o a
    | a == o = [Nothing]
    | Just v <- stripPrefix (o ++ "=") a = [Just v]
    | otherwise = []
  apply a s v = case s of
    SetInt r -> maybe (bad ("wrong argument '" ++ v ++ "'; option '" ++ a ++ "' expects an integer")) (writeIORef r) (readMaybe v)
    SetFloat r -> maybe (bad ("wrong argument '" ++ v ++ "'; option '" ++ a ++ "' expects a float")) (writeIORef r) (readFloat v)
    SetString f -> f v
    Set r -> writeIORef r True
    Clear r -> writeIORef r False
  readFloat v = case readMaybe v of
    Just d -> Just d
    Nothing -> fromIntegral <$> (readMaybe v :: Maybe Int)
