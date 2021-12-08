{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AOC2021D1 ( aoc1 ) where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import Relude

readInt :: Text -> Int -- crash if not an integer
readInt = fromJust . readMaybe . toString . T.filter (/= '+')

aoc1 :: IO (Int, Int)
aoc1 = do
  ss <- readFileText "data/aoc1.dat"
  let is :: [Int]
      is = mapMaybe (readMaybe . toString) $ words ss
  let xxx :: Int -> (Int,Int) -> (Int,Int)
      xxx n (l,s) = if n > l then (n,s+1) else (n,s)
  let resa = foldl' (\(l,s) n -> if n > l then (n,s+1) else (n,s)) (0, -1) is
      (_,a) = resa
      resb = foldl' (\(l0,l1,ll,s) n -> let ll0=l0+l1+n in if ll0>ll then (l1,n,ll0,s+1) else (l1,n,ll0,s)) (0,0,0,-3) is
      (_,_,_,b) = resb
  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)
