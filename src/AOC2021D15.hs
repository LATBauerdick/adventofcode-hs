{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AOC2021D15 ( aoc15 ) where

import qualified Data.List as L
import qualified Relude.Unsafe as Unsafe
import qualified Data.Text as T
import qualified Text.RawString.QQ as TQ

debug :: a -> Text -> a
debug a b = trace (toString b) a

readInt :: Text -> Int -- crash if not an integer
readInt = Unsafe.fromJust . readMaybe . toString . T.filter (/= '+')

data Tree a = Leaf | Node a (Tree a) (Tree a)
summarize :: Tree a -> [[a]]
summarize Leaf = [[]]
summarize (Node a Leaf Leaf) = [[a]]
summarize (Node a t1 t2) = do
  t       <- [t1, t2]
  summary <- summarize t
  return (a:summary)

aoc15 :: IO (Int, Int)
aoc15 = do
  ss <- readFileText "data/aoc15.dat"
  let ss :: Text; ss =
        [TQ.r|1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581|]
  let ls = lines ss
      ll = T.length . Unsafe.head $ ls
      nl = length ls
      tl = nl*ll
      risks = map (\c -> ord c - ord '0') . concatMap toString $ ls

      neighbors :: Int -> (Maybe Int, Maybe Int)
      neighbors i = (r,d) where
                    d = if i < tl-nl then Just (i+ll) else Nothing
                    r = if i `mod` ll /= (ll - 1) then Just (i+1) else Nothing

-- brute force works for the 10x10 example, but blows up for the larger case
      buildNode :: Int -> Int -> Tree Int
      buildNode s i = let a = Unsafe.at i risks; s' = s+a
                       in if s' > nl*ll*3 then Leaf else
                            case neighbors i of
                              (Just r, Just d)   -> Node a (buildNode s' r) (buildNode s' d)
                              (Just r, Nothing)  -> Node a (buildNode s' r) Leaf
                              (Nothing, Just d)  -> Node a Leaf (buildNode s' d)
                              (Nothing, Nothing) -> Node 998 Leaf Leaf

  print risks
  let sm = summarize . buildNode 0 $ 0
  let mn = L.minimum . mapMaybe (\rs -> if Unsafe.last rs == 998 then Just (sum rs - 998) else Nothing) $ sm
  let a = Unsafe.last risks - Unsafe.head risks + mn

-- instead, draw one path and then vary the path only accepting less risky
-- paths?
--

  let b = 1

  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)
