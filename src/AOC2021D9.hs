{-# LANGUAGE OverloadedStrings #-}
--{-# LANGUAGE NoImplicitPrelude #-}

module AOC2021D9 ( aoc9 ) where

import qualified Relude.Unsafe as Unsafe
import qualified Data.List as L ((\\), union)
import qualified Data.Text as T
import Data.Char (digitToInt)
-- import Relude

readInt :: Text -> Int -- crash if not an integer
readInt = Unsafe.fromJust . readMaybe . toString . T.filter (/= '+')

aoc9 :: IO (Int, Int)
aoc9 = do
  ss <- readFileText "data/aoc9.dat"
  let ls = lines ss
      ll = T.length . Unsafe.head $ ls
      nl = length ls
      tl = nl*ll
      pts = concatMap toString ls

  let risk :: Int -> Int
      risk i = let  c = Unsafe.at i pts
                    u = if i > (ll-1) then Unsafe.at (i-ll) pts else 'x'
                    d = if i < tl-nl then Unsafe.at (i+ll) pts else 'x'
                    l = if i `mod` ll /= 0 then Unsafe.at (i-1) pts else 'x'
                    r = if i `mod` ll /= (ll - 1) then Unsafe.at (i+1) pts else 'x'
                in if c<u && c<d && c<l && c<r then 1 + digitToInt c else 0

  let a = sum . map risk $ [0 .. tl - 1]

  let mins :: [Int]
      mins = mapMaybe (\i -> if risk i /= 0 then Just i else Nothing) [0 .. tl - 1]
      neighbors :: Int -> [Int]
      neighbors i = let u = if i > (ll-1) && Unsafe.at (i-ll) pts < '9'
                               then Just (i-ll) else Nothing
                        d = if i < tl-ll && Unsafe.at (i+ll) pts < '9'
                               then Just (i+ll) else Nothing
                        l = if i `mod` ll /= 0 && Unsafe.at (i-1) pts < '9'
                               then Just (i-1)  else Nothing
                        r = if i `mod` ll /= (ll - 1) && Unsafe.at (i+1) pts < '9'
                               then Just (i+1)  else Nothing
                     in catMaybes [u, l, r, d]

      scan :: ([Int], [Int]) -> ([Int], [Int])
      scan (cand, basin) = if null cand' then (cand', basin')
                                         else scan (cand', basin') where
        cand' = foldl' (\cs i -> if i `elem` basin then cs else cs `L.union` neighbors i ) [] cand L.\\ cand
        basin' = sort $ basin `L.union` cand

  -- print . map neighbors $ mins

  let b = foldl' (*) 1 . drop (length mins - 3) . sort
        . map (\i -> length . snd $ scan ([i], [])) $ mins

  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)
