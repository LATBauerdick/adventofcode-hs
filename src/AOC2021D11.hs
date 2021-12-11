
{-# LANGUAGE OverloadedStrings #-}

module AOC2021D11 ( aoc11 ) where

import qualified Relude.Unsafe as Unsafe
import qualified Data.Text as T

readInt :: Text -> Int -- crash if not an integer
readInt = Unsafe.fromJust . readMaybe . toString . T.filter (/= '+')

aoc11 :: IO (Int, Int)
aoc11 = do
  ss <- readFileText "data/aoc11.dat"
  let ls = map toString . lines $ ss
  print ls

  let a = 0; b = 0

  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)
