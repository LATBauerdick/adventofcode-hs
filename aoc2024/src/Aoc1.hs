module Aoc1 (aoc1) where

import qualified Data.List as L (foldl)
import qualified Data.Map as M (Map, empty, findWithDefault, insertWith)
import Data.Maybe (fromJust)
import qualified Data.Text as T (filter, lines, words)

readInt :: Text -> Int -- crash if not an integer
readInt = fromJust . readMaybe . toString . T.filter (/= '+')

slurp :: FilePath -> IO [Text]
-- slurp fn = readFileBS fn <&> T.lines . decodeUtf8
slurp fn = T.lines . decodeUtf8 <$> readFileBS fn

parseLine :: Text -> Maybe (Int, Int)
parseLine line = case T.words line of
  [x, y] -> do
    xInt <- readMaybe $ toString x
    yInt <- readMaybe $ toString y
    return (xInt, yInt)
  _ -> Nothing

aoc1 :: IO (Int, Int)
aoc1 = do
  ls <- slurp "data/aoc1.dat"

  let ps = mapMaybe parseLine ls
      (fs, ss) = unzip ps
      (sfs, sss) = (sort fs, sort ss)
      a = sum $ zipWith (\x y -> abs (x - y)) sfs sss
      xx = ss

  let m :: M.Map Int Int
      m = L.foldl (\m' x -> M.insertWith (+) x 1 m') M.empty ss
      b = L.foldl (\cnt k -> cnt + k * M.findWithDefault 0 k m) 0 fs

  pure (a, b)
