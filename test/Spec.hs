{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main (main) where

import Relude
import Test.Hspec ( Spec, hspec, describe, it, shouldBe )
import Test.QuickCheck ( property )

import AOC2021D8  ( aoc8 )
import AOC2021D7  ( aoc7 )
import AOC2021D6  ( aoc6 )
import AOC2021D5  ( aoc5 )
import AOC2021  ( aoc1
                , aoc2
                , aoc3
                , aoc4
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
      x `shouldBe` (845186,4636702)

    it "test running aoc4" $ do
      x <- aoc4
      x `shouldBe` (60368,17435)

    it "test running aoc5" $ do
      x <- aoc5
      x `shouldBe` (6007,19349)

    it "test running aoc6" $ do
      x <- aoc6
      x `shouldBe` (353274,1609314870967)

    it "test running aoc7" $ do
      x <- aoc7
      x `shouldBe` (344735,96798233)
  
    it "test running aoc8" $ do
      x <- aoc8
      x `shouldBe` (0,1)
