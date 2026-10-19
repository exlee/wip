-- | Splits long words at syllable boundaries for display, adding "-".
-- Heuristic, English-leaning: a lone consonant between vowels starts the next
-- syllable (na-tion); between two or more consonants the break follows the
-- first (in-ter) unless the pair is a digraph (pro-phet).
--
-- Words are Haskell 'String's, so offsets count characters and a break can
-- never land inside a multi-byte UTF-8 sequence.
module Wip.Hyphen (breaks, split, splitM) where

import Data.Char (isAsciiLower, isAsciiUpper, toLower)
import Data.Functor.Identity (Identity (..))
import Data.List (find)

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiouyAEIOUY"

isLetter :: Char -> Bool
isLetter c = isAsciiLower c || isAsciiUpper c

digraphs :: [String]
digraphs = ["ch", "sh", "th", "ph", "wh", "gh"]

-- | Offsets where @w@ may be hyphenated, ascending; each side keeps at
-- least two letters.
breaks :: String -> [Int]
breaks w = scan 0
 where
  n = length w
  at i = w !! i
  vowel i = i < n && isLetter (at i) && isVowel (at i)
  consonant i = i < n && isLetter (at i) && not (isVowel (at i))
  scan i
    | i >= n = []
    | vowel i && consonant (i + 1) =
        let j = until (not . consonant) (+ 1) (i + 1)
            k = j - i - 1
            p
              | k == 1 = i + 1
              | k == 2 && map toLower (take 2 (drop (i + 1) w)) `elem` digraphs = i + 1
              | otherwise = i + 2
            here = [p | vowel j, p >= 2, n - p >= 2]
         in here ++ scan j
    | otherwise = scan (i + 1)

-- | Cuts @w@ into pieces where every piece but the last, followed by "-",
-- satisfies @fits@, preferring syllable breaks and taking as much as fits.
-- Falls back to a plain character break when no syllable break fits.
split :: (String -> Bool) -> String -> [String]
split fits = runIdentity . splitM (Identity . fits)

-- | 'split' with an effectful test, e.g. measuring rendered text width.
splitM :: Monad m => (String -> m Bool) -> String -> m [String]
splitM fits w = go 0
 where
  syllables = breaks w
  chars = [1 .. length w - 1]
  piece start p = take (p - start) (drop start w)
  findM _ [] = pure Nothing
  findM f (x : xs) = f x >>= \b -> if b then pure (Just x) else findM f xs
  go start = do
    let rest = drop start w
        pieceFits p = fits (piece start p ++ "-")
        best candidates = findM pieceFits (reverse (filter (> start) candidates))
        continue p = (piece start p :) <$> go p
    whole <- fits rest
    if whole
      then pure [rest]
      else
        best syllables >>= \case
          Just p -> continue p
          Nothing ->
            best chars >>= \case
              Just p -> continue p
              -- Not even one character fits: emit one anyway so this ends.
              Nothing -> maybe (pure [rest]) continue (find (> start) chars)
