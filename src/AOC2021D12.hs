{-# LANGUAGE OverloadedStrings #-}

module AOC2021D12 ( aoc12 ) where

import Data.Tree as DT
import qualified Relude.Unsafe as Unsafe
import qualified Data.Text as T

-- debug :: a -> Text -> a
-- debug a b = trace (toString b) a

readInt :: Text -> Int -- crash if not an integer
readInt = Unsafe.fromJust . readMaybe . toString . T.filter (/= '+')

aoc12 :: IO (Int, Int)
aoc12 = do
  -- ss <- readFileText "data/aoc12.dat"

  -- let ns = M.fromList [ ("start", ["A","b"]), ("A",["c","b","end"]),("b",["A","end"]),("c",["A"]) ]
  -- let buildNode x = remove  . Unsafe.fromJust . M.lookup x $ ns

  --print . DT.foldTree (\x xs -> ) DT.unfoldTree buildNode $ "start"
  -- putStr $ DT.drawTree $ show <$> DT.unfoldTree buildNode "start"

  let a = 0; b = 0

  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)
