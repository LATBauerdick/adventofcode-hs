module Aoc5 (aoc5) where

import Data.List as L (and, break, length, (!!))
import qualified Data.Map.Strict as M (empty, insert, insertWith, keys, lookup, toList)
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

reordered :: Map Int [Int] -> [Int] -> Int
reordered mrules ps = c
 where
  c =
    if all
      ( \(f, s) ->
          not ((f `elem` ps) && (any (`elem` s) . takeWhile (/= f) $ ps))
      )
      $ M.toList mrules
      then 0
      else reorder mrules ps !! (length ps `div` 2)
reorder :: Map Int [Int] -> [Int] -> [Int]
reorder mrs = sortBy cmp
 where
  cmp :: Int -> Int -> Ordering
  cmp a b = case M.lookup a mrs of
    Nothing -> EQ
    Just bs -> if b `elem` bs then GT else EQ

mapRules :: [(Int, Int)] -> Map Int [Int]
mapRules = foldl' (\m (a, b) -> M.insertWith (++) a [b] m) M.empty

aoc5 :: IO (Int, Int)
aoc5 = do
  ls' <- slurp "data/aoc5.dat" <&> take 1176 . drop 28
  ls <- slurp "data/aoc5.dat" <&> drop 1205
  -- ls' <- slurp "data/aoc5.dat" <&> take 21
  -- ls <- slurp "data/aoc5.dat" <&> take 6 . drop 22
  let rs =
        fmap
          ( bimap readInt (readInt . T.drop 1)
              . T.breakOn "|"
          )
          ls'

  let ps = fmap (mapMaybe readMaybeInt . T.splitOn ",") ls
  let a = sum . fmap (checkRules rs) $ ps

  let mrs = mapRules rs
  let b = sum . fmap (reordered mrs) $ ps
  pure (a, b)
