module Aoc5 (aoc5) where

import Data.List as L (and, break, length, (!!))
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

checkRules :: [(Int, Int)] -> [Int] -> Int
checkRules rules ps = c
 where
  c =
    if all
      ( \(f, s) ->
          not ((f `elem` ps) && (elem s . takeWhile (/= f) $ ps))
      )
      rules
      then ps !! (length ps `div` 2)
      else 0

reordered :: [(Int, Int)] -> [Int] -> Int
reordered rules ps = c
 where
  c =
    if all
      ( \(f, s) ->
          not ((f `elem` ps) && (elem s . takeWhile (/= f) $ ps))
      )
      rules
      then 0
      else 1

aoc5 :: IO (Int, Int)
aoc5 = do
  -- ls' <- slurp "data/aoc5.dat" <&> take 1176 . drop 28
  -- ls <- slurp "data/aoc5.dat" <&> drop 1205
  ls' <- slurp "data/aoc5.dat" <&> take 21
  ls <- slurp "data/aoc5.dat" <&> take 6 . drop 22
  let rs =
        fmap
          ( bimap readInt (readInt . T.drop 1)
              . T.breakOn "|"
          )
          ls'

  let ps = fmap (mapMaybe readMaybeInt . T.splitOn ",") ls
  print rs
  print ps

  print . fmap (checkRules rs) $ ps

  let a = sum . fmap (checkRules rs) $ ps

  print . fmap (reordered rs) $ ps

  let b = sum . fmap (reordered rs) $ ps
  pure (a, b)
