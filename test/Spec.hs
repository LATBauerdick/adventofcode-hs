{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main (main) where

import Relude
import Test.Hspec ( Spec, hspec, describe, it, shouldBe )
import Test.QuickCheck ( property )

import AOC2020  ( aoc1
                , aoc2
                , aoc3
                , aoc4
                , aoc5
                , aoc6
                , aoc7
                , aoc8
                , aoc9
                )
main :: IO ()

main = do
  hspec spec

spec :: Spec
spec =
  describe "AOC 2020" $ do
    it "test running aoc1" $ do
      x <- aoc1
      x `shouldBe` (41979, 193416912)

    it "test running aoc2" $ do
      x <- aoc2
      x `shouldBe` (600, 245)

    it "test running aoc3" $ do
      x <- aoc3
      x `shouldBe` (242, 2265549792)

    it "test running aoc4" $ do
      x <- aoc4
      x `shouldBe` (226, 160)

    it "test running aoc5" $ do
      x <- aoc5
      x `shouldBe` (906, 519)

    it "test running aoc6" $ do
      x <- aoc6
      x `shouldBe` (6387, 3039)

    it "test running aoc7" $ do
      x <- aoc7
      x `shouldBe` (233, 421550)

    it "test running aoc8" $ do
      x <- aoc8
      x `shouldBe` (1548, 1375)

    it "test running aoc9" $ do
      x <- aoc9
      x `shouldBe` (1639024365, 219202240)

