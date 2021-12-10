{-# LANGUAGE OverloadedStrings #-}

module AOC2021D7 ( aoc7 ) where

import Data.Maybe (fromJust)
import qualified Data.List as L (minimum, maximum)
import qualified Data.Text as T

readInt :: Text -> Int -- crash if not an integer
readInt = fromJust . readMaybe . toString . T.filter (/= '+')

aoc7 :: IO (Int, Int)
aoc7 = do
  ss <- readFileText "data/aoc7.dat"
  let pos:: [Int]; pos = readInt <$> T.splitOn "," ss
  let mn = L.minimum pos; mx = L.maximum pos; axis = [mn .. mx]

  let fuelConsumption :: [Int] -> Int -> Int
      fuelConsumption ps npos = sum . map (\i -> abs (npos - i)) $ ps

  let a = L.minimum $ map (fuelConsumption pos) [mn .. mx]

  let fuelConsumption' :: [Int] -> Int -> Int
      fuelConsumption' ps npos = sum . map (\i -> let d = abs (npos - i) in d*(d+1) `div` 2) $ ps
  let b = L.minimum $ map (fuelConsumption' pos) [mn .. mx]

  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)
