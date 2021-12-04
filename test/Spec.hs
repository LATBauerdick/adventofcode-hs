{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main (main) where

import Relude
import Test.Hspec ( Spec, hspec, describe, it, shouldBe )
import Test.QuickCheck ( property )

import AOC2021  ( aoc1
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


