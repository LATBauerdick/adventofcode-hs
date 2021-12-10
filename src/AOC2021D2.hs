{-# LANGUAGE OverloadedStrings #-}

module AOC2021D2 ( aoc2 ) where

import Data.Maybe (fromJust)
import qualified Data.Text as T

readInt :: Text -> Int -- crash if not an integer
readInt = fromJust . readMaybe . toString . T.filter (/= '+')

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

  let b = let (p,d,_) = coor' in p*d
  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)
