{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AOC2021  ( aoc1
                , aoc2
                , aoc3
                , aoc4
                , aoc5
                ) where

import Data.Char (digitToInt)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
-- import qualified Data.List as L (drop, head, maximum, minimum, take, last, (!!))
-- import qualified Data.List.Split as L (chunksOf)
-- import qualified Data.List.Unique as L (repeatedBy, sortUniq)
import qualified Data.Map.Strict as M (insertWith, empty, filter, size)
import qualified Data.Text as T
import Relude

newtype Bag = Bag Text deriving (Eq)

data BagRule = BagRule Bag [Bag] Bool

type Point = (Int, Int)
type Segment = (Point, Point)
readInt :: Text -> Int -- crash if not an integer
readInt = fromJust . readMaybe . toString . T.filter (/= '+')

aoc5 :: IO (Int, Int)
aoc5 = do -- thanks for parsing help to https://github.com/BartoszMilewski
  ss <- readFileText "data/aoc5.dat"
  let segs = parseLn . words <$> lines ss
      parseLn [sp1, _, sp2] =
              let
                  [x0,y0] = T.splitOn "," sp1
                  [x1,y1] = T.splitOn "," sp2
              in ((readInt x0, readInt y0), (readInt x1, readInt y1))
  let notDiag :: Segment -> Bool; notDiag ((x0,y0),(x1,y1)) = x0 == x1 || y0 == y1
  let mkLine :: Segment -> [Point]
      mkLine ((x0, y0), (x1, y1)) =
        let dx = abs ( x1-x0 ); dy = abs ( y1-y0 ); ddx = signum ( x1-x0 ); ddy = signum ( y1-y0 )
        in [ (x0 + n * ddx, y0 + n * ddy) | n <- [0 .. max dx dy]]

  let a =  M.size . M.filter (>1) . foldl' (\m p -> M.insertWith (+) p 1 m) M.empty . concatMap mkLine . filter notDiag $ segs
  let b =  M.size . M.filter (>1) . foldl' (\m p -> M.insertWith (+) p 1 m) M.empty . concatMap mkLine $ segs

  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)

aoc4 :: IO (Int, Int)
aoc4 = do
  ss <- readFileText "data/aoc4.dat"
  let 
      draws :: [Int]
      draws = mapMaybe (readMaybe . toString) . T.split (==',') . fromJust . viaNonEmpty head $ lines ss
      boards :: [[Int]]
      boards = chunksOf 25 . mapMaybe (readMaybe . toString) . drop 1 $ words ss
  -- print draws
  -- print boards
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

  print $ winner boards


  let marked :: [[Int]] -> Int -> [[Int]]
      marked bs draw = map (mark draw) bs
      mark :: Int -> [Int] -> [Int]
      mark d bs = map (\i -> if i == d then -1 else i) bs

  let (_, winnerBoard, lastDraw) = foldl' (\(bs, w, l) d -> let ms = marked bs d; win = winner ms in if w /= [] then ([],w,l) else if win /= [] then ([], win, d) else (ms, [], 0)) (boards, [], 0) draws
  print winnerBoard
  print lastDraw
  print $ score winnerBoard

  let losers :: [[Int]] -> [[Int]]
      losers = mapMaybe (\b -> if hasWon b then Nothing else Just b )
  let (_, loserDraw, loserBoard) =
        foldl' (\(rembs, ld, lb) d -> let ms = marked rembs d; ls = losers ms in if null rembs then ([],ld,lb) else if null ls then ([],d,ms) else (ls,d,lb)) (boards, 0, []) draws

  print loserBoard
  -- print $ losers loserBoard
  -- print $ viaNonEmpty head loserBoard
  print loserDraw
  let loserScore = score . fromJust $ viaNonEmpty head loserBoard
  -- print loserScore

  let a = lastDraw * score winnerBoard; b = loserDraw * loserScore
  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)

aoc3 :: IO (Int, Int)
aoc3 = do
  ss <- readFileText "data/aoc3.dat"
  let ws = words ss
      bs :: [Int]
      bs = foldl' (\bits cs ->
                    let bits'  = map (\x -> if x == '1' then 1 else 0) cs
                      in zipWith (+) bits bits'
                  ) (replicate 12 0) . map toString $ ws
  print bs
  let n2 = length ws `div` 2
      gammab = map (\b -> if b > n2 then '1' else '0') bs
      epsb   = map (\b -> if b < n2 then '1' else '0') bs
      toDec :: String -> Int
      toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0
  print (gammab, toDec gammab)
  print (epsb, toDec epsb)

  let a = toDec gammab * toDec epsb

  let o2 :: Int -> [Text] -> Char
      o2 i wws =
        let cnt = foldl' (\x w -> if T.index w i == '1' then x+1 else x) 0 wws
            n = length wws
         in if cnt >= n-cnt then '1' else '0'
  let co2 :: Int -> [Text] -> Char
      co2 i wws =
        let cnt = foldl' (\x w -> if T.index w i == '1' then x+1 else x) 0 wws
            n = length wws
         in if cnt < n-cnt then '1' else '0'
      xxx :: (Int -> [Text] -> Char) -> [Text] -> Int -> [Text]
      xxx fun wws i =
        let b = fun i wws
         in if length wws == 1 then wws else mapMaybe (\w -> if T.index w i == b then Just w else Nothing) wws

  let ogr = toDec . toString . fromMaybe "0" $ viaNonEmpty head (foldl' (xxx o2) ws $ take 12 [0..])
  let csr = toDec . toString . fromMaybe "0" $ viaNonEmpty head (foldl' (xxx co2) ws $ take 12 [0..])

  print ogr
  print csr
{-
  let neu = xxx co2 ws 0
  let neu1 = xxx co2 neu 1
  let neu2 = xxx co2 neu1 2
  let neu3 = xxx co2 neu2 3
  let neu4 = xxx co2 neu3 4
  print neu
  print neu1
  print neu2
  print neu3
  print neu4
-}
  let b = ogr*csr
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
