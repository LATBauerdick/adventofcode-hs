{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AOC2021 ( aoc1
               ) where

import qualified Data.List as L (drop, head, maximum, minimum, take, last, (!!))
import qualified Data.List.Split as L (chunksOf)
import qualified Data.List.Unique as L (repeatedBy, sortUniq)
import qualified Data.Map.Strict as M (fromList, keys, lookup, toList, insert, empty)
import qualified Data.Text as T
import Relude

newtype Bag = Bag Text deriving (Eq)

data BagRule = BagRule Bag [Bag] Bool

aoc1 :: IO (Int, Int)
aoc1 = do
  -- response <- httpLBS "https://adventofcode.com/2020/day/1/input"
  -- putStrLn $ "status code: " ++ show (getResponseStatusCode response)
  -- L8.putStrLn $ getResponseBody response
  ss <- readFileText "data/aoc1.dat"
  let is :: [Int]
      is = mapMaybe (readMaybe . toString) $ words ss
  print is
  let xxx :: Int -> (Int,Int) -> (Int,Int)
      xxx n (l,s) = if n > l then (n,s+1) else (n,s)
  let resa = foldl' (\(l,s) n -> if n > l then (n,s+1) else (n,s)) (0, -1) is
      (_,a) = resa
      resb = foldl' (\(l0,l1,ll,s) n -> let ll0=l0+l1+n in if ll0>ll then (l1,n,ll0,s+1) else (l1,n,ll0,s)) (0,0,0,-3) is
      (_,_,_,b) = resb
  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)
