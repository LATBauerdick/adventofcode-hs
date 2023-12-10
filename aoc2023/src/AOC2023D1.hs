module AOC2023D1 ( aoc1 ) where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Char (isDigit)

readInt :: Text -> Int -- crash if not an integer
readInt = fromJust . readMaybe . toString . T.filter (/= '+')

tokens :: [Text]
tokens = ["1","2","3","4","5","6","7","8","9"
         ,"one","two","three","four","five","six","seven","eight","nine"]
values :: [Int]
values = [1..9] <> [1..9]

aoc1 :: IO (Int, Int)
aoc1 = do
  ss <- readFileText "data/aoc1.dat"
  let is :: [Int]
      is = map (\(c, _) -> 10* ( ord c - ord '0' ))
         . mapMaybe T.uncons
         . map ( T.filter isDigit )
         $ T.words ss
      iis :: [Int]
      iis = map (\(_, c) -> ( ord c - ord '0' ))
          . mapMaybe T.unsnoc
          . map ( T.filter isDigit )
          $ T.words ss
  let a = sum is + sum iis

  let f :: Text -> Int
      f t = (10 *) . snd . fromJust
          . viaNonEmpty head
          . sortOn fst
          . map (\(x, i) ->  (T.length . fst $ T.breakOn x t, i))
          $ zip tokens values
  let l :: Text -> Int
      l t = snd . fromJust
          . viaNonEmpty head
          . sortOn fst
          . map (\(x, i) ->  (T.length . snd $ T.breakOnEnd x t, i))
          $ zip tokens values
  let ii = map ( \x -> f x + l x ) $ T.words ss
  let b = sum ii
  -- putTextLn $ "debug " <> show ii
  putTextLn $ "result is " <> show (a,b)
  pure (a, b)
