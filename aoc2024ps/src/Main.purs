module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Aoc1 ( aoc1 )

main :: Effect Unit
main = do
  x <- aoc1
  log $ show x

