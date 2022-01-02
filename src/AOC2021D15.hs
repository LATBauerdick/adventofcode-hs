{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AOC2021D15 ( aoc15 ) where

import qualified Data.List as L
import qualified Relude.Unsafe as Unsafe
import qualified Data.Text as T
import qualified Text.RawString.QQ as TQ
import qualified Data.Map.Strict as M
import qualified Data.Set as S


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
  let ss0 :: Text; ss0 =
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

-- brute force works for the 10x10 example, but blows up for the larger case
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

  print risks
  let
      neighbors' :: Int -> [Int]
      neighbors' i = catMaybes $ [u,l,r,d] where
                    u = if i > (ll-1) then Just (i-ll) else Nothing
                    d = if i < tl-nl then Just (i+ll) else Nothing
                    l = if i `mod` ll /= 0 then Just (i-1) else Nothing
                    r = if i `mod` ll /= (ll - 1) then Just (i+1) else Nothing
      large :: Int; large = 99999999
      nodes :: Set (Int,Int)
      nodes = S.insert (0,0) $ S.fromList [(large,n) | n <- [1..(tl-1)] ]
      nextNode :: (Set (Int,Int), Set(Int,Int)) -> (Set (Int,Int),Set (Int,Int))
      nextNode (ns,nvs) =
        let (d,i) = Unsafe.fromJust . S.lookupGE (0,0) $ ns
            thisNode :: Int -> (Int,Int)
            thisNode ii = Unsafe.fromJust . S.lookupGE (ii,0) $ nvs
            notVisited ii = case S.lookupGE (ii,0) nvs of
                      Nothing -> Nothing
                      Just (iii,_) -> if iii == ii then Just iii else Nothing
            nbs :: [Int]
            nbs = mapMaybe notVisited . neighbors' $ i
            ns' = foldr (\ii nns -> let (_,r) = thisNode ii
                                        (dd,_) = Unsafe.fromJust . S.lookupGE (0,0) . S.filter (\(_,iii) -> iii==ii) $ nns
                                     in if d+r<dd
                                           then S.insert (d+r,ii) . S.delete (dd,ii) $ nns
                                           else nns
                        ) ns $ nbs
         in if i == tl-1
              then (S.singleton (d,i), S.empty)
              else if notVisited i == Just i
                then (S.delete (d,i) ns', S.delete (thisNode i) nvs)
                else (S.delete (d,i) ns, nvs)
      notVisited :: Set (Int,Int)
      notVisited = S.fromList . zip [0..] $ risks
  print (nodes, notVisited)
  print . nextNode $ (nodes, notVisited)
  print . nextNode . nextNode $ (nodes, notVisited)
  print . foldr (\_ (ns,nvs) -> nextNode (ns,nvs)) (nodes,notVisited) $ [0..(2*tl-1)]
  let a = fst . Unsafe.fromJust . S.lookupGE (0,0) . fst . foldr (\_ (ns,nvs) -> nextNode (ns,nvs)) (nodes,notVisited) $ [0..(2*tl-1)]
  --
  -- let sm = summarize . buildNode 0 $ 0
  -- let mn = L.minimum . mapMaybe (\rs -> if Unsafe.last rs == 998 then Just (sum rs - 998) else Nothing) $ sm
  -- let a = Unsafe.last risks - Unsafe.head risks + mn

-- instead, draw one path and then vary the path only accepting less risky
-- paths?
--

  let b = 1

  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)
