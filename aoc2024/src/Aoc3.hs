module Aoc3 (aoc3) where

-- stripPrefix
-- splitOn "aaa"  "aaaXaaaXaaaXaaa" -> ["","X","X","X",""]
-- breakOnAll "/" "a/b/c/" -> [("a","/b/c/"),("a/b","/c/"),("a/b/c","/")]
-- uncons :: Text -> Maybe (Char, Text) Returns the first character and rest of a Text, or Nothing if empty.

import qualified Data.List as L (foldl, foldl1, foldr1, length)
import Data.Maybe (fromJust)
import qualified Data.Text as T (breakOnAll, filter, lines, splitOn)

debug :: a -> Text -> a
debug a t = flip trace a (toString t)

slurp :: FilePath -> IO [Text]
slurp fn = T.lines . decodeUtf8 <$> readFileBS fn

readInt :: Text -> Int -- crash if not an integer
readInt = fromJust . readMaybe . toString . T.filter (/= '+')

readMaybeInt :: Text -> Maybe Int
readMaybeInt = readMaybe . toString . T.filter (/= '+')

readMaybeInts :: Text -> [Maybe Int]
readMaybeInts l = map readMaybeInt $ T.splitOn " " l

dropNth :: [a] -> Int -> [a]
dropNth xs n = take n xs ++ drop (n + 1) xs

-------------------------------------------------------------------

parseLine :: Text -> Int
parseLine l = p
 where
  terms' = T.splitOn "mul(" l
  terms = T.breakOnAll "mul(" l
  p = 0 `debug` show terms

aoc3 :: IO (Int, Int)
aoc3 = do
  ls <- slurp "data/aoc3.dat" <&> take 1
  print . parseLine . fromJust . viaNonEmpty head $ ls
  let a = sum . fmap parseLine $ ls
  let b = sum . fmap parseLine $ ls

  pure (a, b)
