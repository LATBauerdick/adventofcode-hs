module AOC2023D1 ( aoc1 ) where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Char (isDigit)

readInt :: Text -> Int -- crash if not an integer
readInt = fromJust . readMaybe . toString . T.filter (/= '+')

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
  let b = a
  -- putTextLn $ "debug " <> show is
  putTextLn $ "result is " <> show (a,b)
  pure (a, b)
