-- | Minimal JSON value, parser and printer.
module Wip.Json
  ( Json (..)
  , parse
  , render
  , member
  , toStringOpt
  , toIntOpt
  ) where

import Data.Char (chr, isDigit, isHexDigit, ord)
import Numeric (readHex, showHex)
import Text.Read (readMaybe)

data Json
  = Null
  | Bool Bool
  | Num Double
  | Str String
  | Arr [Json]
  | Obj [(String, Json)]
  deriving (Eq, Show)

-- Parser state: position (for error messages) and remaining input.
newtype P a = P {runP :: Int -> String -> Either String (a, Int, String)}

instance Functor P where
  fmap f (P p) = P $ \pos s -> fmap (\(a, pos', s') -> (f a, pos', s')) (p pos s)

instance Applicative P where
  pure a = P $ \pos s -> Right (a, pos, s)
  pf <*> pa = pf >>= \f -> fmap f pa

instance Monad P where
  P p >>= k = P $ \pos s -> case p pos s of
    Left e -> Left e
    Right (a, pos', s') -> runP (k a) pos' s'

failP :: String -> P a
failP msg = P $ \pos _ -> Left (msg ++ " at " ++ show pos)

-- '\0' at the end of input, like a C string.
peek :: P Char
peek = P $ \pos s -> Right (case s of c : _ -> c; [] -> '\0', pos, s)

advance :: Int -> P ()
advance k = P $ \pos s -> Right ((), pos + k, drop k s)

atEnd :: P Bool
atEnd = P $ \pos s -> Right (null s, pos, s)

rest :: P String
rest = P $ \pos s -> Right (s, pos, s)

skipWs :: P ()
skipWs = peek >>= \c -> if c `elem` " \t\n\r" then advance 1 >> skipWs else pure ()

expect :: Char -> P ()
expect c = peek >>= \d -> if d == c then advance 1 else failP ("expected '" ++ [c] ++ "'")

literal :: String -> Json -> P Json
literal word v = do
  s <- rest
  if take (length word) s == word then advance (length word) >> pure v else failP "invalid literal"

hex4 :: P Int
hex4 = do
  s <- rest
  let digits = take 4 s
  if length digits < 4 then failP "short \\u escape" else pure ()
  advance 4
  case readHex digits of
    [(v, "")] | all isHexDigit digits -> pure v
    _ -> failP "bad \\u escape"

parseString :: P String
parseString = expect '"' >> loop []
 where
  loop acc = do
    end <- atEnd
    if end then failP "unterminated string" else pure ()
    c <- peek
    advance 1
    case c of
      '"' -> pure (reverse acc)
      '\\' -> do
        e <- peek
        advance 1
        ch <- case e of
          '"' -> pure '"'
          '\\' -> pure '\\'
          '/' -> pure '/'
          'b' -> pure '\b'
          'f' -> pure '\f'
          'n' -> pure '\n'
          'r' -> pure '\r'
          't' -> pure '\t'
          'u' -> do
            hi <- hex4
            next <- peek
            if hi >= 0xD800 && hi < 0xDC00 && next == '\\'
              then do
                advance 1
                expect 'u'
                lo <- hex4
                pure (chr (0x10000 + ((hi - 0xD800) * 1024) + (lo - 0xDC00)))
              else pure (chr hi)
          _ -> failP "bad escape"
        loop (ch : acc)
      _ -> loop (c : acc)

parseNumber :: P Json
parseNumber = do
  s <- rest
  let digits = takeWhile (`elem` "0123456789-+.eE") s
  advance (length digits)
  maybe (failP "bad number") (pure . Num) (readDouble digits)

-- | Accepts what C's strtod does for the characters above: optional sign,
-- digits with an optional fraction (either side may be empty), optional
-- exponent.
readDouble :: String -> Maybe Double
readDouble s0 = case s0 of
  '-' : r -> negate <$> unsigned r
  '+' : r -> unsigned r
  r -> unsigned r
 where
  unsigned r =
    let (mantissa, ex) = break (`elem` "eE") r
        (ip, fp) = break (== '.') mantissa
        frac = drop 1 fp
        expo = case drop 1 ex of
          '+' : d -> Just d
          '-' : d -> Just ('-' : d)
          d -> Just d
     in if null ip && null frac || not (all isDigit (ip ++ frac)) || take 2 fp == ".." then Nothing
          else case (null ex, expo) of
            (True, _) -> readMaybe (orZero ip ++ "." ++ orZero frac)
            (False, Just e) | validExp e -> readMaybe (orZero ip ++ "." ++ orZero frac ++ "e" ++ e)
            _ -> Nothing
  orZero x = if null x then "0" else x
  validExp e = case e of
    '-' : d -> not (null d) && all isDigit d
    d -> not (null d) && all isDigit d

value :: P Json
value = do
  skipWs
  c <- peek
  case c of
    '{' -> do
      advance 1
      skipWs
      close <- peek
      if close == '}' then advance 1 >> pure (Obj []) else members []
    '[' -> do
      advance 1
      skipWs
      close <- peek
      if close == ']' then advance 1 >> pure (Arr []) else elems []
    '"' -> Str <$> parseString
    't' -> literal "true" (Bool True)
    'f' -> literal "false" (Bool False)
    'n' -> literal "null" Null
    _ -> parseNumber
 where
  members acc = do
    skipWs
    k <- parseString
    skipWs
    expect ':'
    v <- value
    skipWs
    c <- peek
    case c of
      ',' -> advance 1 >> members ((k, v) : acc)
      '}' -> advance 1 >> pure (Obj (reverse ((k, v) : acc)))
      _ -> failP "expected ',' or '}'"
  elems acc = do
    v <- value
    skipWs
    c <- peek
    case c of
      ',' -> advance 1 >> elems (v : acc)
      ']' -> advance 1 >> pure (Arr (reverse (v : acc)))
      _ -> failP "expected ',' or ']'"

-- | Parses a whole document; Left carries the error and its position.
parse :: String -> Either String Json
parse s = fmap (\(v, _, _) -> v) (runP document 0 s)
 where
  document = do
    v <- value
    skipWs
    end <- atEnd
    if end then pure v else failP "trailing data"

escape :: String -> ShowS
escape s = showChar '"' . foldr (\c k -> esc c . k) id s . showChar '"'
 where
  esc c = case c of
    '"' -> showString "\\\""
    '\\' -> showString "\\\\"
    '\n' -> showString "\\n"
    '\r' -> showString "\\r"
    '\t' -> showString "\\t"
    _ | ord c < 0x20 -> showString "\\u" . showString (pad4 (showHex (ord c) ""))
    _ -> showChar c
  pad4 h = replicate (4 - length h) '0' ++ h

render :: Json -> String
render v = go v ""
 where
  go j = case j of
    Null -> showString "null"
    Bool b -> showString (if b then "true" else "false")
    Num f
      | isWhole f && abs f < 1e15 -> shows (truncate f :: Integer)
      | otherwise -> shows f
    Str s -> escape s
    Arr l -> showChar '[' . commaSep (map go l) . showChar ']'
    Obj l -> showChar '{' . commaSep [escape k . showChar ':' . go x | (k, x) <- l] . showChar '}'
  commaSep [] = id
  commaSep (x : xs) = x . foldr (\y k -> showChar ',' . y . k) id xs

isWhole :: Double -> Bool
isWhole f = not (isNaN f || isInfinite f) && f == fromInteger (truncate f)

member :: String -> Json -> Maybe Json
member k (Obj l) = lookup k l
member _ _ = Nothing

toStringOpt :: Maybe Json -> Maybe String
toStringOpt (Just (Str s)) = Just s
toStringOpt _ = Nothing

toIntOpt :: Maybe Json -> Maybe Int
toIntOpt (Just (Num f)) | isWhole f = Just (truncate f)
toIntOpt _ = Nothing
