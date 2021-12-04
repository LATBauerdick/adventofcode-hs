{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main (main) where

import Relude
import Test.Hspec ( Spec, hspec, describe, it, shouldBe )
import Test.QuickCheck ( property )

import AOC2021  ( aoc1
                , aoc2
                , aoc3
                )
main :: IO ()

main = do
  hspec spec

spec :: Spec
spec =
  describe "AOC 2021" $ do
    it "test running aoc1" $ do
      x <- aoc1
      x `shouldBe` (1665,1702)

    it "test running aoc2" $ do
      x <- aoc2
      x `shouldBe` (1480518,1282809906)

    it "test running aoc3" $ do
      x <- aoc3
      x `shouldBe` (1480518,1282809906)
