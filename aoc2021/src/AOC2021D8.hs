{-# LANGUAGE OverloadedStrings #-}

module AOC2021D8 ( aoc8 ) where

import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M (insertWith, empty, elems, restrictKeys)
import qualified Data.Set as S (fromList)
import qualified Data.List as L (sort, intersect, find, lookup, length)
import qualified Data.Text as T

readInt :: Text -> Int -- crash if not an integer
readInt = fromJust . readMaybe . toString . T.filter (/= '+')

aoc8 :: IO (Int, Int)
aoc8 = do
  ss <- readFileText "data/aoc8.dat"
  let outputs = foldl'(\m t -> M.insertWith (+) (T.length t) 1 m) M.empty . concatMap (drop 11 . words) $ lines ss
  let uniqueOutputs = M.restrictKeys  outputs $ S.fromList [2, 3, 4, 7]

  let a = sum $ M.elems uniqueOutputs

  let getCodes :: [Text] -> [(Text, Int)]
      getCodes ts = foldl' patterns []
                  . map (toText . L.sort . toString)
                  . sortOn T.length $ ts where
        patterns :: [(Text, Int)] -> Text -> [(Text, Int)]
        patterns m t =
          case T.length t of
                    2 -> (t, 1) : m
                    3 -> (t, 7) : m
                    4 -> (t, 4) : m
                    5 -> let p7 = toString . fst . fromJust . L.find (\p -> snd p == 7) $ m
                             p4 = toString . fst . fromJust . L.find (\p -> snd p == 4) $ m
                          in if (p7 `L.intersect` toString t) == p7 then (t, 3) : m
                          else if L.length (p4 `L.intersect` toString t)  == 2 then (t, 2) : m
                          else (t, 5) : m
                    6 -> let p7 = toString . fst . fromJust . L.find (\p -> snd p == 7) $ m
                             p3 = toString . fst . fromJust . L.find (\p -> snd p == 3) $ m
                          in if p7 `L.intersect` toString t /= p7 then (t, 6) : m
                          else if (p3 `L.intersect` toString t) == p3 then (t, 9) : m
                          else (t, 0) : m
                    7 -> (t, 8) : m
                    _ -> m

      decode :: [Text] -> [(Text, Int)] -> [Int]
      decode ts pat = map (\t -> fromJust $ L.lookup (toText . L.sort . toString $ t) pat) ts

      calc :: [Int] -> Int
      calc = fst . foldr (\i (s, f) -> (s+i*f, f*10)) (0,1)

      doLine :: [Text] -> Int
      doLine ws = calc . decode (drop 11 ws) . getCodes . take 10 $ ws

  let b = sum . map (doLine . words) $ lines ss

  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)
