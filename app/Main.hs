{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}

module Main where

import Relude

import AOC2021D1 ( aoc1
               )

main :: IO ()
main = aoc1 >>= print
