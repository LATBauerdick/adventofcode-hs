{-# LANGUAGE OverloadedStrings #-}

module AOC2021D5 ( aoc5
                 ) where

import qualified Relude.Unsafe as Unsafe
import qualified Data.Map.Strict as M (insertWith, empty, filter, size)
import qualified Data.Text as T

type Point = (Int, Int)
type Segment = (Point, Point)
readInt :: Text -> Int -- crash if not an integer
readInt = Unsafe.fromJust . readMaybe . toString . T.filter (/= '+')

aoc5 :: IO (Int, Int)
aoc5 = do -- thanks for parsing help to https://github.com/BartoszMilewski
  ss <- readFileText "data/aoc5.dat"
  let segs = parseLn . words <$> lines ss
      parseLn [sp1, _, sp2] =
              let
                  [x0,y0] = T.splitOn "," sp1
                  [x1,y1] = T.splitOn "," sp2
              in ((readInt x0, readInt y0), (readInt x1, readInt y1))
      parseLn _ = error "bad input from data/aoc5.dat"
  let notDiag :: Segment -> Bool; notDiag ((x0,y0),(x1,y1)) = x0 == x1 || y0 == y1
  let mkLine :: Segment -> [Point]
      mkLine ((x0, y0), (x1, y1)) =
        let dx = abs ( x1-x0 ); dy = abs ( y1-y0 ); ddx = signum ( x1-x0 ); ddy = signum ( y1-y0 )
        in [ (x0 + n * ddx, y0 + n * ddy) | n <- [0 .. max dx dy]]

  let a =  M.size . M.filter (> (1::Int)) . foldl' (\m p -> M.insertWith (+) p 1 m) M.empty . concatMap mkLine . filter notDiag $ segs
  let b =  M.size . M.filter (> (1::Int)) . foldl' (\m p -> M.insertWith (+) p 1 m) M.empty . concatMap mkLine $ segs

  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)
