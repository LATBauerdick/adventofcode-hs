{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck (property)

import Aoc1 (aoc1)
import Aoc2 (aoc2)
import Aoc3 (aoc3)
import Aoc4 (aoc4)
import Aoc5 (aoc5)
import Aoc6 (aoc6)

main :: IO ()
main = do
  hspec spec

spec :: Spec
spec =
  describe "AOC 2024" $ do
    it "-----------test running aoc6" $ do
      x <- aoc6
      x `shouldBe` (0, 0)
    it "-----------test running aoc5" $ do
      x <- aoc5
      x `shouldBe` (7307, 4713)
    it "-----------test running aoc4" $ do
      x <- aoc4
      x `shouldBe` (2493, 1890)
    it "-----------test running aoc3" $ do
      x <- aoc3
      x `shouldBe` (157621318, 79845780)
    it "-----------test running aoc2" $ do
      x <- aoc2
      x `shouldBe` (356, 413)
    it "-----------test running aoc1" $ do
      x <- aoc1
      x `shouldBe` (1319616, 27267728)
