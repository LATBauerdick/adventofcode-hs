module Aoc4 (aoc4) where

import Data.List as L (length)
import Data.Maybe (fromJust)
import qualified Data.Text as T (breakOn, breakOnAll, drop, filter, intercalate, length, lines, splitOn, transpose)

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

aoc4 :: IO (Int, Int)
aoc4 = do
  ls <- slurp "data/aoc4.dat" <&> take 10
  print ls
  let ll = T.length . fromJust . viaNonEmpty head $ ls
      nl = length ls
      tl = nl * ll
  let vs = T.transpose ls
  print vs
  let xs = [[(min j (ll - 1), min i (nl - 1)) | j <- [0 .. i]] | i <- [0 .. (ll + nl - 1)]]
  print xs
  print . fmap parseLine $ ls
  print . fmap parseLine $ vs

  let a = sum $ (fmap parseLine ls) <> (fmap parseLine vs)
  let b = 0
  pure (a, b)
