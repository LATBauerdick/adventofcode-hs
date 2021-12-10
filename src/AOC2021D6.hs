
{-# LANGUAGE OverloadedStrings #-}

module AOC2021D6 ( aoc6
                 ) where

import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M (elems, insert, fromList, adjust)
import qualified Data.Text as T

readInt :: Text -> Int -- crash if not an integer
readInt = fromJust . readMaybe . toString . T.filter (/= '+')

aoc6 :: IO (Int, Int)
aoc6 = do
  ss <- readFileText "data/aoc6.dat"
  let lfs:: [Int]; lfs = readInt <$> T.splitOn "," ss

  let step :: [Int] -> [Int]; step = foldr (\f fs -> if f == 0 then 8 : 6 : fs else f-1 : fs) []
  -- print . step $ lfs
  let allfs = foldl' (\fs _ -> step fs) lfs $ replicate 80 (0 :: Int)
  -- print allfs

  let a = length allfs

-- this approach explodes and cannot calculate step 256...
-- so we count the fish in each different phase (0..8) and update the numbers in
-- each step
  let cnts :: Map Int Int; cnts = M.fromList $ zip [0..8] (replicate 9 (0 :: Int))
  let iter :: Map Int Int -> Map Int Int
      iter m = 
        let
            n0 : ns = M.elems m
            m0 =  M.fromList $ zip [0..7] ns
            m1 = M.insert 8 n0 m0
            m2 = M.adjust (+ n0) 6 m1
        in m2
  let step0 :: Map Int Int; step0 = foldr (\i m -> M.adjust (+1) i m) cnts lfs
  let steps = take (80 + 1) $ iterate iter step0 -- +1 because fist entry is the original state
  -- print . sum . M.elems . fromJust $ viaNonEmpty last steps
  let manysteps = take (256 + 1) $ iterate iter step0

  let b = sum . M.elems . fromJust $ viaNonEmpty last manysteps

  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)
