{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AOC2021D4  ( aoc4 ) where

import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Relude

aoc4 :: IO (Int, Int)
aoc4 = do
  ss <- readFileText "data/aoc4.dat"
  let 
      draws :: [Int]
      draws = mapMaybe (readMaybe . toString) . T.split (==',') . fromJust . viaNonEmpty head $ lines ss
      boards :: [[Int]]
      boards = chunksOf 25 . mapMaybe (readMaybe . toString) . drop 1 $ words ss
  let hasWon :: [Int] -> Bool
      hasWon board = not (null board) &&
                        let [  a00, a01, a02, a03, a04
                             , a10, a11, a12, a13, a14
                             , a20, a21, a22, a23, a24
                             , a30, a31, a32, a33, a34
                             , a40, a41, a42, a43, a44
                             ] = board
                          in  a00+a01+a02+a03+a04 == -5
                                  || a10+a11+a12+a13+a14 == -5
                                  || a20+a21+a22+a23+a24 == -5
                                  || a30+a31+a32+a33+a34 == -5
                                  || a40+a41+a42+a43+a44 == -5
                                  || a00+a10+a20+a30+a40 == -5
                                  || a01+a11+a21+a31+a41 == -5
                                  || a02+a12+a22+a32+a42 == -5
                                  || a03+a13+a23+a33+a43 == -5
                                  || a04+a14+a24+a34+a44 == -5

      winner :: [[Int]] -> [Int]
      winner = foldl' (\wb b -> if wb /= [] then wb else if hasWon b then b else [] ) []
      score :: [Int] -> Int
      score board = if hasWon board then foldl' (\s i -> if i>0 then s+i else s ) 0 board else 0

  -- print $ winner boards


  let marked :: [[Int]] -> Int -> [[Int]]
      marked bs draw = map (mark draw) bs
      mark :: Int -> [Int] -> [Int]
      mark d bs = map (\i -> if i == d then -1 else i) bs

  let (_, winnerBoard, lastDraw) = foldl' (\(bs, w, l) d -> let ms = marked bs d; win = winner ms in if w /= [] then ([],w,l) else if win /= [] then ([], win, d) else (ms, [], 0)) (boards, [], 0) draws

  let losers :: [[Int]] -> [[Int]]
      losers = mapMaybe (\b -> if hasWon b then Nothing else Just b )
  let (_, loserDraw, loserBoard) =
        foldl' (\(rembs, ld, lb) d -> let ms = marked rembs d; ls = losers ms in if null rembs then ([],ld,lb) else if null ls then ([],d,ms) else (ls,d,lb)) (boards, 0, []) draws

  let loserScore = score . fromJust $ viaNonEmpty head loserBoard

  let a = lastDraw * score winnerBoard; b = loserDraw * loserScore
  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)

