module Main where

import Prelude
import Data.Tuple

import Effect (Effect)
import Effect.Console (log)

import Aoc3 ( aoc3 )
import Aoc2 ( aoc2 )
import Aoc1 ( aoc1 )

shouldBe :: Tuple Int Int -> Tuple Int Int -> Effect Unit --String
shouldBe t1 t0 = log $ case t0 == t1 of
  true -> "Success: " <> show t0
  false -> "Error: Result " <> show t0 <> " should be " <> show t1

main :: Effect Unit
main = do
  aoc1 >>= shouldBe (Tuple 1319616 27267728)
  aoc2 >>= shouldBe (Tuple 356 413)
  aoc3 >>= shouldBe (Tuple 0 0)
