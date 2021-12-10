{-# LANGUAGE OverloadedStrings #-}

module AOC2021D10 ( aoc10 ) where

import qualified Relude.Unsafe as Unsafe
import qualified Data.Text as T

readInt :: Text -> Int -- crash if not an integer
readInt = Unsafe.fromJust . readMaybe . toString . T.filter (/= '+')

aoc10 :: IO (Int, Int)
aoc10 = do
  ss <- readFileText "data/aoc10.dat"

  let a = 0; b = 0

  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)
