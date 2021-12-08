
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AOC2021D8 ( aoc8 ) where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import Relude

readInt :: Text -> Int -- crash if not an integer
readInt = fromJust . readMaybe . toString . T.filter (/= '+')

aoc8 :: IO (Int, Int)
aoc8 = do
  ss <- readFileText "data/aoc8.dat"

  let a = 0; b = 0

  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)
