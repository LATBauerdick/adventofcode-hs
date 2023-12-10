{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec ( Spec, hspec, describe, it, shouldBe )
import Test.QuickCheck ( property )

import AOC2023D1  ( aoc1 )

main :: IO ()

main = do
  hspec spec

spec :: Spec
spec =
  describe "AOC 2023" $ do
    it "test running aoc1" $ do
      x <- aoc1
      x `shouldBe` (55488, 55614)

