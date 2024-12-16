module Aoc3 (aoc3) where

-- stripPrefix
-- splitOn "aaa"  "aaaXaaaXaaaXaaa" -> ["","X","X","X",""]
-- breakOnAll "/" "a/b/c/" -> [("a","/b/c/"),("a/b","/c/"),("a/b/c","/")]
-- breakOn "::" "a::b::c" -> ("a","::b::c")
-- uncons :: Text -> Maybe (Char, Text) Returns the first character and rest of a Text, or Nothing if empty.

import qualified Data.List as L (foldl, foldl1, foldr1, length)
import Data.Maybe (fromJust)
import qualified Data.Text as T (breakOn, breakOnAll, drop, filter, lines, splitOn)

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
terms :: Text -> [Int]
terms =
  mapMaybe
    ( (\(a, b) -> (*) <$> a <*> b)
        . bimap readMaybeInt (readMaybeInt . T.drop 1)
        . T.breakOn ","
        . fst
        . T.breakOn ")"
        . T.drop 4
        . snd
    )
    . T.breakOnAll "mul("

parseLine :: Text -> Int
parseLine = sum . terms

parseLine' :: Text -> Int
parseLine' l = p
 where
  dropToDo :: Text -> Text
  dropToDo = T.drop 4 . snd . T.breakOn "do()"
  reduce :: ([Int], Text) -> ([Int], Text)
  reduce (t, r) = bimap ((++) t . terms) dropToDo . T.breakOn "don't()" $ r
  -- (t0, r0) = reduce ([], l)
  -- (t1, r1) = reduce (t0, r0)
  -- (t2, r2) = reduce (t1, r1)
  doReduce :: ([Int], Text) -> ([Int], Text)
  doReduce (t, r)
    | r == "" = (t, r) `debug` show (t, sum t, r)
    | otherwise = doReduce . reduce $ (t, r) -- `debug` show (t, sum t, r)
  p = sum . fst . doReduce $ ([], l)

aoc3 :: IO (Int, Int)
aoc3 = do
  ls <- slurp "data/aoc3.dat" <&> drop 1
  print . parseLine' . fromJust . viaNonEmpty head $ ls
  let a = sum . fmap parseLine $ ls
  let b = sum . fmap parseLine' $ ls
  print . fmap parseLine' $ ls
  pure (a, b)

-- 15583044
-- 18644260
-- 8788325
-- 19282189
-- 19951739
-- 5294156
--
