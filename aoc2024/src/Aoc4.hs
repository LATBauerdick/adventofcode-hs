module Aoc4 (aoc4) where

import Data.Maybe (fromJust)
import qualified Data.Text as T (breakOn, breakOnAll, drop, filter, intercalate, lines, splitOn)

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
aoc4 :: IO (Int, Int)
aoc4 = do
  ls <- slurp "data/aoc4.dat" <&> take 10
  let a = 0
  let b = 0
  pure (a, b)
