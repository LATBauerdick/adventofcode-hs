{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck (property)

import Aoc1 (aoc1)

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec =
  describe "AOC 2024" $ do
    it "-----------test running aoc1" $ do
      x <- aoc1
      x `shouldBe` (1319616, 27267728)
