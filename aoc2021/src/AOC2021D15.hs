{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AOC2021D15 ( aoc15 ) where

import qualified Relude.Unsafe as Unsafe
import qualified Data.Text as T
import qualified Text.RawString.QQ as TQ
import qualified Data.Set as S

-- debug :: a -> Text -> a
-- debug a b = trace (toString b) a

readInt :: Text -> Int -- crash if not an integer
readInt = Unsafe.fromJust . readMaybe . toString . T.filter (/= '+')

path :: [Int] -> (Int -> [Int]) -> Int
path grid nb = len where
  tl = length grid
  pq :: Set (Int,Int); pq = S.singleton (0,0)
  nextNode :: (Set (Int,Int), Set Int) -> (Set (Int,Int), Set Int)
  nextNode (ns,vs) =
    let (d,i) = Unsafe.fromJust . S.lookupGE (0,0) $ ns
        nbs :: [Int]
        nbs = mapMaybe (\ii -> if S.member ii vs then Nothing else Just ii) . nb $ i
        ns' = foldr (\ii nns -> let r = Unsafe.at ii grid
                                in S.insert (d+r,ii) nns
                    ) ns nbs
    in if i == tl-1
          then (S.singleton (d,i), S.empty)
          else if S.member i vs
            then (S.delete (d,i) ns, vs)
            else (S.delete (d,i) ns', S.insert i vs)
  visited :: Set Int
  visited = S.empty
  len = fst . Unsafe.fromJust . S.lookupGE (0,0) . fst . foldr (\_ (ns,vs) -> nextNode (ns,vs)) (pq,visited) $ [0..(2*tl-1)]
-- N.B., replacing Set with a "real" prioty queue implementation
-- (MinQueue (Int,Int) from Data.PQueue.Min) actually increases the execution time!

neighbors :: Int -> Int -> Int -> [Int]
neighbors ll nl i = catMaybes [u,l,r,d] where
              u = if i > (ll-1) then Just (i-ll) else Nothing
              d = if i < (ll-1)*nl then Just (i+ll) else Nothing
              l = if i `mod` ll /= 0 then Just (i-1) else Nothing
              r = if i `mod` ll /= (ll - 1) then Just (i+1) else Nothing

aoc15 :: IO (Int, Int)
aoc15 = do
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
  -- ss <- readFileText "data/aoc15.dat"
  let ls = lines ss
      ll = T.length . Unsafe.head $ ls
      nl = length ls
      risks = map (\c -> ord c - ord '0') . concatMap toString $ ls

  let nb = neighbors ll nl
      a = path risks nb

  let tod :: Text -> [Int]; tod = map (\c -> ord c - ord '0') . toString
      off o = map (\i -> let x = i+o in if x>9 then x-9 else x)
      m0 = concat . foldr (\ds xs -> off 0 ds : off 1 ds : off 2 ds : off 3 ds : off 4 ds : xs) [] . map tod $ ls
      risks' = concat [off 0 m0, off 1 m0, off 2 m0, off 3 m0, off 4 m0]

  let nb' = neighbors (5*ll) (5*nl)
      b = path risks' nb'

  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)
--
-- brute force works for the 10x10 example, but blows up for the larger case
--
-- data Tree a = Leaf | Node a (Tree a) (Tree a)
-- summarize :: Tree a -> [[a]]
-- summarize Leaf = [[]]
-- summarize (Node a Leaf Leaf) = [[a]]
-- summarize (Node a t1 t2) = do
--   t       <- [t1, t2]
--   summary <- summarize t
--   return (a:summary)

-- neighbors :: Int -> (Maybe Int, Maybe Int)
-- neighbors i = (r,d) where
--               d = if i < tl-nl then Just (i+ll) else Nothing
--               r = if i `mod` ll /= (ll - 1) then Just (i+1) else Nothing
-- buildNode :: Int -> Int -> Tree Int
-- buildNode s i = let a = Unsafe.at i risks; s' = s+a
--                  in if s' > nl*ll*3 then Leaf else
--                       case neighbors i of
--                         (Just r, Just d)   -> Node a (buildNode s' r) (buildNode s' d)
--                         (Just r, Nothing)  -> Node a (buildNode s' r) Leaf
--                         (Nothing, Just d)  -> Node a Leaf (buildNode s' d)
--                         (Nothing, Nothing) -> Node 998 Leaf Leaf
--
-- let sm = summarize . buildNode 0 $ 0
-- let mn = L.minimum . mapMaybe (\rs -> if Unsafe.last rs == 998 then Just (sum rs - 998) else Nothing) $ sm
-- let a = Unsafe.last risks - Unsafe.head risks + mn
