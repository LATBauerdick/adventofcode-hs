module Aoc5 (aoc5) where

import Data.List as L (length, (!!))
import Data.Maybe (Maybe, catMaybes, fromJust, mapMaybe)
import qualified Data.Text as T (breakOn, breakOnAll, drop, filter, index, intercalate, length, lines, splitOn, transpose)

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

-------------------------------------------------------------------

parseLine :: Text -> Int
parseLine l = n
 where
  n = n0 + n1
  n0 = L.length . T.breakOnAll "XMAS" $ l
  n1 = L.length . T.breakOnAll "SAMX" $ l

aoc5 :: IO (Int, Int)
aoc5 = do
  ls <- slurp "data/aoc5.dat" <&> drop 28
  let a = 0
  let b = 0
  pure (a, b)
