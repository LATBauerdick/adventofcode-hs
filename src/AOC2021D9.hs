{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AOC2021D9 ( aoc9 ) where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import Relude

readInt :: Text -> Int -- crash if not an integer
readInt = fromJust . readMaybe . toString . T.filter (/= '+')

aoc9 :: IO (Int, Int)
aoc9 = do
  ss <- readFileText "data/aoc9.dat"

  let a = 0; b = 0

  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)
