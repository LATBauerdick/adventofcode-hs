module Aoc3 (aoc3) where

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
parseLine' l = sum . fst . reduce $ ([], l)
 where
  dropToDo :: Text -> Text
  dropToDo = T.drop 4 . snd . T.breakOn "do()"
  go :: ([Int], Text) -> ([Int], Text)
  go (t, r) = bimap ((++) t . terms) dropToDo . T.breakOn "don't()" $ r
  reduce :: ([Int], Text) -> ([Int], Text)
  reduce (t, r)
    | r == "" = (t, r) -- `debug` show (t, sum t, r)
    | otherwise = reduce . go $ (t, r) -- `debug` show (t, sum t, r)

aoc3 :: IO (Int, Int)
aoc3 = do
  ls <- slurp "data/aoc3.dat" <&> drop 1
  let a = sum . fmap parseLine $ ls
  let b = parseLine' . T.intercalate "" $ ls
  pure (a, b)
