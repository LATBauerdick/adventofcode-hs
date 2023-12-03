{-# LANGUAGE OverloadedStrings #-}

module AOC2021D12 ( aoc12 ) where

import qualified Relude.Unsafe as Unsafe
import qualified Data.Text as T

debug :: a -> Text -> a
debug a b = trace (toString b) a

readInt :: Text -> Int -- crash if not an integer
readInt = Unsafe.fromJust . readMaybe . toString . T.filter (/= '+')

aoc12 :: IO (Int, Int)
aoc12 = do
  ss <- readFileText "data/aoc12.dat"

  let a = 0; b = 0

  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)
