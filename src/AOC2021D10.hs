{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AOC2021D10 ( aoc10 ) where

import qualified Relude.Unsafe as Unsafe
import qualified Data.Text as T
import Relude

readInt :: Text -> Int -- crash if not an integer
readInt = Unsafe.fromJust . readMaybe . toString . T.filter (/= '+')

data Stack a = Stack [a] deriving Show

stEmpty :: Stack a
stEmpty = Stack []
push :: a -> Stack a -> Stack a
push x (Stack xs)= Stack (x:xs)
pop :: Stack a -> (Maybe a, Stack a)
pop (Stack []) = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)

aoc10 :: IO (Int, Int)
aoc10 = do
  ss <- readFileText "data/aoc10.dat"
  let ls = map toString . lines $ ss

  let xxx :: String; xxx = "{([(<{}[<>[]}>{[]{[(<()>"
      -- xxx = Unsafe.head . drop 2 $ ls
  -- print xxx

  let isOpening :: Char -> Bool; isOpening c = c `elem` ['<','[','(','{']
  let isClosing :: Char -> Char -> Bool; isClosing o c = case o of
                                                           '<' -> c == '>'
                                                           '[' -> c == ']'
                                                           '(' -> c == ')'
                                                           '{' -> c == '}'
                                                           _ -> False
  let value :: Char -> Int; value c = case c of
                                        ')' -> 3
                                        ']' -> 57
                                        '}' -> 1197
                                        '>' -> 25137
                                        _ -> 99999
  let parse :: String -> (Stack Char, [Maybe Int])
      parse cs = foldl' (\(st, l) c -> if isOpening c then (push c st, Nothing : l)
                                                   else case pop st of
                                                          (Just o, st') ->
                                                             if isClosing o c then (st', Nothing : l) else (st', Just ( value c ): l)
                                                          _ -> (stEmpty , error "bad input aoc10" )
                        ) (stEmpty, []) $ cs
  let res = map parse ls

  let a = sum . concatMap (catMaybes . snd) $ res

  let values :: [Char] -> Int; values cs =  foldl' (\s c -> s*5 + (case c of
                                                                   '('-> 1
                                                                   '['-> 2
                                                                   '{'-> 3
                                                                   '<'-> 4
                                                                   _ -> error "bad input aoc10")
                                                    ) 0 $ cs
  let vs = sort . map values . mapMaybe (\(aa,bb) -> if catMaybes bb == [] then let Stack s = aa in Just s else Nothing)  $ res

  let b = Unsafe.head . drop (length vs `div` 2) $ vs

  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)
