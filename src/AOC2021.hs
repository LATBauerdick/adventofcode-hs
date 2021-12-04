{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AOC2021  ( aoc1
                , aoc2
                , aoc3
                ) where

-- import qualified Data.List as L (drop, head, maximum, minimum, take, last, (!!))
-- import qualified Data.List.Split as L (chunksOf)
-- import qualified Data.List.Unique as L (repeatedBy, sortUniq)
-- import qualified Data.Map.Strict as M (fromList, keys, lookup, toList, insert, empty)
import qualified Data.Text as T
import Relude

newtype Bag = Bag Text deriving (Eq)

data BagRule = BagRule Bag [Bag] Bool

aoc3 :: IO (Int, Int)
aoc3 = do
  ss <- readFileText "data/aoc3.dat"

  let a = 0
      b = 0
  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)

aoc2 :: IO (Int, Int)
aoc2 = do
  ss <- readFileText "data/aoc2.dat"
  let ls = lines ss
      coord :: (Int,Int)
      coord = foldl' (\(pos,dep) line ->
                        let [dir,amount] = words line
                          in case dir of
                            "forward" -> (pos + (fromMaybe 0 . readMaybe . toString . T.filter (/= '+') $ amount), dep)
                            "down" -> (pos, dep + (fromMaybe 0 . readMaybe . toString . T.filter (/= '+') $ amount))
                            "up" -> (pos, dep - (fromMaybe 0 . readMaybe . toString . T.filter (/= '+') $ amount))
                            _ -> error "wrong input reading aoc2.dat"
                      ) (0,0) ls
  print coord
  let a = uncurry (*) coord
      coor' :: (Int,Int,Int)
      coor' = foldl' (\(pos,dep,aim) line ->
                        let [dir,amount] = words line
                          in case dir of
                            "forward" ->  let x=fromMaybe 0 . readMaybe . toString . T.filter (/= '+') $ amount
                                            in (pos+x , dep + aim*x, aim)
                            "down" -> (pos, dep, aim + (fromMaybe 0 . readMaybe . toString . T.filter (/= '+') $ amount))
                            "up" -> (pos, dep, aim - (fromMaybe 0 . readMaybe . toString . T.filter (/= '+') $ amount))
                            _ -> error "wrong input reading aoc2.dat"
                      ) (0,0,0) ls
  print coor'
  let b = let (p,d,_) = coor' in p*d
  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)

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
