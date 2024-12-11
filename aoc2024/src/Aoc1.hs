module Aoc1 (aoc1) where

import Data.Maybe (fromJust)
import qualified Data.Text as T

-- import Data.Char (isDigit)

readInt :: Text -> Int -- crash if not an integer
readInt = fromJust . readMaybe . toString . T.filter (/= '+')

parseLine :: Text -> Maybe (Int, Int)
parseLine line = case T.words line of
  [x, y] -> do
    xInt <- readMaybe $ toString x
    yInt <- readMaybe $ toString y
    return (xInt, yInt)
  _ -> Nothing

aoc1 :: IO (Int, Int)
aoc1 = do
  fc <- readFileBS "data/aoc1.dat"
  -- testHSlurp =<< readFileText "data/aoc1.dat"
  -- print . T.lines $ decodeUtf8 fc
  let ps = mapMaybe parseLine . T.lines $ decodeUtf8 fc
  let (fs, ss) = unzip ps
  let (sfs, sss) = (sort fs, sort ss)
  let a = sum $ zipWith (\x y -> abs (x - y)) sfs sss

  -- let a = sum . fmap (\(a,b) -> a+b ). zip .

  -- putTextLn $ "debug " <> show ii
  -- putTextLn $ "result is " <> show (a,b)
  pure (a, 0)
