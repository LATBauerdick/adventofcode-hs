{-# LANGUAGE OverloadedStrings #-}

module AOC2021D3 ( aoc3 ) where

import Data.Char (digitToInt)
import Data.Maybe (fromJust)
import qualified Data.Text as T

readInt :: Text -> Int -- crash if not an integer
readInt = fromJust . readMaybe . toString . T.filter (/= '+')

aoc3 :: IO (Int, Int)
aoc3 = do
  ss <- readFileText "data/aoc3.dat"
  let ws = words ss
      bs :: [Int]
      bs = foldl' (\bits cs ->
                    let bits'  = map (\x -> if x == '1' then 1 else 0) cs
                      in zipWith (+) bits bits'
                  ) (replicate 12 0) . map toString $ ws
  let n2 = length ws `div` 2
      gammab = map (\b -> if b > n2 then '1' else '0') bs
      epsb   = map (\b -> if b < n2 then '1' else '0') bs
      toDec :: String -> Int
      toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

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

  let b = ogr*csr
  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)


