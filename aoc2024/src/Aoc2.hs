module Aoc2 (aoc2) where

import qualified Data.List as L (foldl, foldl1, foldr1, length)
import Data.Maybe (fromJust)
import qualified Data.Text as T (filter, lines, splitOn)

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

isSafe :: [Maybe Int] -> Bool
isSafe is = isJust safe
 where
  safe = eith is
  -- return nothing if neither a nor b is Just
  eith s = min <$> a <*> b <|> a <|> b
   where
    a = ascending s
    b = descending s
    ascending = L.foldl1 asc
    descending = L.foldr1 (flip asc)
    asc :: Maybe Int -> Maybe Int -> Maybe Int
    asc (Just l) (Just i) = if i > l && i - l < 4 then Just i else Nothing
    asc _ _ = Nothing

parseLine :: Text -> Bool
parseLine = isSafe . readMaybeInts

parseLine1 :: Text -> Bool
parseLine1 line = any isSafe iis
 where
  is = readMaybeInts line
  len = L.length is
  iis = map (dropNth is) [0 .. len]

  dropNth :: [a] -> Int -> [a]
  dropNth xs n = take n xs ++ drop (n + 1) xs

aoc2 :: IO (Int, Int)
aoc2 = do
  ls <- slurp "data/aoc2.dat" <&> drop 6
  let a = L.foldl (\cnt i -> if i then cnt + 1 else cnt) 0 . fmap parseLine $ ls
  let b = L.foldl (\cnt i -> if i then cnt + 1 else cnt) 0 . fmap parseLine1 $ ls

  pure (a, b)
