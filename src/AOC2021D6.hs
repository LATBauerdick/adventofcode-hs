
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AOC2021D6 ( aoc6
                 ) where

import Data.Char (digitToInt)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M (insertWith, empty, filter, size)
import qualified Data.Text as T
import Relude

type Point = (Int, Int)
type Segment = (Point, Point)
readInt :: Text -> Int -- crash if not an integer
readInt = fromJust . readMaybe . toString . T.filter (/= '+')

aoc6 :: IO (Int, Int)
aoc6 = do
  ss <- readFileText "data/aoc6.dat"

  let a = 1
  let b = 0

  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)
