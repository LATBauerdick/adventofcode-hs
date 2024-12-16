{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck (property)

import Aoc3 (aoc3)
import Aoc2 (aoc2)
import Aoc1 (aoc1)

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec =
  describe "AOC 2024" $ do
    it "-----------test running aoc3" $ do
      x <- aoc3
      x `shouldBe` (157621318, 79845780)
    it "-----------test running aoc2" $ do
      x <- aoc2
      x `shouldBe` (356, 413)
    it "-----------test running aoc1" $ do
      x <- aoc1
      x `shouldBe` (1319616, 27267728)
