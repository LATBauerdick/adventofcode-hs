{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AOC2020 ( aoc1
  , aoc2
  , aoc3
  , aoc4
  , aoc5
  , aoc6
  , aoc7
  , aoc8
  , aoc9
               ) where

import qualified Data.List as L (drop, head, maximum, minimum, take, last, (!!))
import qualified Data.List.Split as L (chunksOf)
import qualified Data.List.Unique as L (repeatedBy, sortUniq)
import qualified Data.Map.Strict as M (fromList, keys, lookup, toList, insert, empty)
import qualified Data.Text as T
import Relude

newtype Bag = Bag Text deriving (Eq)

data BagRule = BagRule Bag [Bag] Bool

aoc9 :: IO (Int, Int)
aoc9 = do
  ss <- readFileText "data/aoc9.dat"
  let ws :: [Int]; ws = mapMaybe ( readMaybe . toString ) . words $ ss
  let n = 25
  let hasPair :: [Int] -> Int -> Int -> Maybe Int
      hasPair xs x _ = do
        guard $ notElem x [(xs L.!! i) + (xs L.!! j) | i<- [0..n-1], j <- [0..n-1], i/=j]
        pure x
  -- print . mapMaybe (\off -> hasPair (L.take n . L.drop off $ ws) (ws L.!! (n+off)) off) $ [0 .. length ws - n - 1]
  let a = L.head . mapMaybe (\off -> hasPair (L.take n . L.drop off $ ws) (ws L.!! (n+off)) off) $ [0 .. length ws - n - 1]

  let sumUp :: [Int] -> Int -> Maybe Int
      sumUp xs x = do
        let accum :: (Int,Int) -> Maybe (Int, (Int, Int))
            accum (s, cnt) = do
              let c = xs L.!! cnt
              let s' = s + c
              guard $ s' <= x
              pure (c, (s', cnt+1))
            cs = unfoldr accum (0, 0)
        guard $ length cs > 1
        guard $ sum cs == x
        let mx = L.maximum cs
        let mn = L.minimum cs
        pure $ mn + mx
  -- print (a, ws)
  let b = L.head . mapMaybe (\i -> sumUp (L.drop i ws) a) $ [0..length ws - 1]
  putTextLn $ "result is " <> show (a, b)
  pure (a, b)

aoc8 :: IO (Int, Int)
aoc8 = do
  ss <- readFileText "data/aoc8.dat"
  let ls = lines ss
      instrs :: [(Int,Int)]
      instrs = map (\i -> case T.take 3 i of
                            "nop" -> (0, 0)
                            "acc" -> (1, fromMaybe 0 . readMaybe . toString . T.filter (/= '+') . T.drop 4 $ i)
                            "jmp" -> (2, fromMaybe 0 . readMaybe . toString . T.filter (/= '+') . T.drop 4 $ i)
                            _ -> (9, 9))
                            $ lines ss
  let exe :: (Int, Int, Map Int Int) -> Maybe (Int, (Int, Int, Map Int Int))
      exe (cnt, acc, im) = do
        inst <- (!!?) instrs cnt
        -- let inst = (L.!!) instrs cnt
        let acc' = case fst inst of
                1 -> acc + snd inst
                _ -> acc
            cnt' = case fst inst of
                 2 -> cnt + snd inst
                 _ -> cnt+1
            im' = M.insert cnt acc im
        guard $ isNothing (M.lookup cnt' im)
        pure (acc', (cnt', acc', im'))
  -- print $ unfoldr exe (0, 0, M.empty)
  let a = L.last $ unfoldr exe (0, 0, M.empty)

  let exe' :: (Int, Int, Int, Map Int Int) -> Maybe ((Int, Int), (Int, Int, Int, Map Int Int))
      exe' (cnt, acc, chg, im) = do
        inst' <- (!!?) instrs cnt
        let inst = if cnt == chg then (case fst inst' of -- change one inst
                                        0 -> (2, snd inst')
                                        2 -> (0, snd inst')
                                        _ -> inst'
                                     )
                                else inst'
        let acc' = case fst inst of
                1 -> acc + snd inst
                _ -> acc
            cnt' = case fst inst of
                 2 -> cnt + snd inst
                 _ -> cnt+1
            im' = M.insert cnt acc im
            chg' = chg
        guard $ isNothing (M.lookup cnt' im)
        pure ((acc', cnt'), (cnt', acc', chg', im'))
  let execute i = res where
        xs = unfoldr exe' (0, 0, i, M.empty)
        x = L.last xs
        res = if snd x == length instrs then Just (x,xs) else Nothing
  -- forM_ [0..length instrs] (print . execute)
  let b = fst . fst . L.head . mapMaybe execute $ [0..length instrs]
  putTextLn $ "result is " <> show (a, b)
  pure (a, b)

aoc7 :: IO (Int, Int)
aoc7 = do
  ss <- readFileText "data/aoc7.dat"
  let ls = lines ss
  let bg = Bag "shiny gold"
  let parse :: Text -> (Text, Map Text Int)
      parse l = res
        where
          cs = L.chunksOf 4 . words $ l
          name = T.unwords . L.take 2 $ L.head cs
          conts = M.fromList . map (\x -> (T.unwords . L.take 2 . L.drop 1 $ x, fromMaybe 0 $ readMaybe . toString . L.head $ x)) $ drop 1 cs
          res = (name, conts)
  let bags :: Map Text (Map Text Int)
      bags = M.fromList . map parse $ ls
  -- print . mapMaybe (\x -> do
  --                           _ <- M.lookup "shiny gold" . snd $ x
  --                           pure . fst $ x)
  --       . M.toList $ bags
  let flatten :: Map Text Int -> Map Text Int
      flatten bm = res
        where
          nextBags :: Map Text Int -> Map Text Int
          nextBags ms = newMap
            where
              bs = M.keys ms
              x1 = concatMap M.keys . mapMaybe (`M.lookup` bags) $ bs
              x0 = M.fromList $ zip (bs <> x1) [1 ..]
              newMap =
                if (isJust . M.lookup "shiny gold" $ x0)
                  || ((length . M.keys) x0 == length bs)
                  then x0
                  else nextBags x0
          newBags = nextBags bm
          res = newBags
  let openAllAndSearchGold :: (Text, Map Text Int) -> (Text, Map Text Int)
      openAllAndSearchGold x = (fst x, flatten . snd $ x)
  let allbags = map openAllAndSearchGold . M.toList $ bags
  -- print $ L.take 5 . map (\x -> "<" <> fst x <> "> -> " <>
  --   (show . M.keys . snd $ x)) $ allbags
  -- print . mapMaybe (\x -> do
  --                           _ <- M.lookup "shiny gold" . snd $ x
  --                           pure . fst $ x)
  --       $ allbags
  -- print . L.take 5 $ allbags
  let a = length . mapMaybe (M.lookup "shiny gold" . snd) $ allbags
  let count :: Map Text Int -> Int
      count bm = sum 
                . map ( \b -> snd b * (1 + maybe 0 count (M.lookup (fst b) bags)))
                $ M.toList bm
  let b = count $ fromMaybe (error "no shiny gold bag!") (M.lookup "shiny gold" bags)
  putTextLn $ "result is " <> show (a, b)
  pure (a, b)

aoc6 :: IO (Int, Int)
aoc6 = do
  -- cat data/aoc6.dat| fmt -w 99999 | tr -s '\n' > data/aoc6.pdat
  ss <- readFileText "data/aoc6.pdat"
  let ls = lines ss

  let a = sum . map (T.length . T.filter (' ' /=) . toText . L.sortUniq . toString) $ ls
  let cnt l = length . L.repeatedBy (== n) . toString $ l
        where
          n = length . words $ l
  -- print $ map cnt ls
  let b = sum . map cnt $ ls
  putTextLn $ "result is " <> show (a, b)
  pure (a, b)

aoc5 :: IO (Int, Int)
aoc5 = do
  ss <- readFileText "data/aoc5.dat"
  let ws = words ss
  let bin :: Text -> Text
      bin = T.map uncode
        where
          uncode x
            | x == 'L' = '0'
            | x == 'R' = '1'
            | x == 'F' = '0'
            | x == 'B' = '1'
            | otherwise = 'x'
      bin2dec :: Text -> Int
      bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i . toString
        where
          c2i c = if c == '0' then 0 else 1

  let seats = map (\w -> (bin2dec . bin . T.take 7 $ w, bin2dec . bin . T.drop 7 $ w))
  -- print $ seats $ drop (length ws - 5) ws
  let a = L.maximum . map (\x -> fst x * 8 + snd x) . seats $ ws
      a' = L.minimum . map (\x -> fst x * 8 + snd x) . seats $ ws
  -- print $ sort . map (\x -> fst x * 8 + snd x) . seats  $ ws
  let b = sum [a' .. a] - (sum . map (\x -> fst x * 8 + snd x) . seats $ ws)
  putTextLn $ "result is " <> show (a, b)
  pure (a, b)

aoc4 :: IO (Int, Int)
aoc4 = do
  --  cat data/aoc4.dat| fmt -w 99999 | tr -s '\n' > aoc4.pdat
  ss <- readFileText "data/aoc4.pdat"
  let xxx :: [Text] -> Bool
      xxx = (\xs -> (length xs == 7 && notElem "cid" xs) || length xs == 8) . map (T.take 3)
      a = length . filter id . map (xxx . words) $ lines ss
  let perLine = zzz . M.fromList . map (\w -> (T.take 3 w, T.drop 4 w))
      zzz :: Map Text Text -> Maybe Text
      zzz z = do
        let readText :: Text -> Maybe Int
            readText t = readMaybe . toString $ t
        tb <- M.lookup "byr" z
        b <- readText tb
        guard $ b >= 1920
        guard $ b <= 2002
        ti <- M.lookup "iyr" z
        i <- readText ti
        guard $ i >= 2010
        guard $ i <= 2020
        te <- M.lookup "eyr" z
        e <- readText te
        guard $ e >= 2020
        guard $ e <= 2030
        th <- M.lookup "hgt" z
        let uh = T.takeEnd 2 th
        h <- readText $ T.dropEnd 2 th
        guard $ case uh of
          "in" -> h >= 59 && h <= 76
          "cm" -> h >= 150 && h <= 193
          _ -> False
        thc <- M.lookup "hcl" z
        guard $ T.length thc == 7
        guard $ T.head thc == '#'
        guard $ T.filter (`notElem` (['0' .. '9'] <> ['a' .. 'f'])) (T.tail thc) == ""
        tec <- M.lookup "ecl" z
        guard $ elem tec ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        pid <- M.lookup "pid" z
        guard $ T.length pid == 9
        _ <- readText pid
        pure pid

  -- print $ map (perLine . words) $ lines ss
  let b = length . mapMaybe (perLine . words) $ lines ss
  putTextLn $ "result is " <> show (a, b)
  pure (a, b)

aoc3 :: IO (Int, Int)
aoc3 = do
  ss <- readFileText "data/aoc3.dat"
  let tree :: Int -> Int -> Text -> Bool
      tree inc ln line = T.index line i == '#'
        where
          i = (ln * inc) `mod` 31

  let a :: Int
      a = sum $ (\p -> if p then 1 else 0) <$> snd (mapAccumL (\cnt line -> (cnt + 1, tree 3 cnt line)) 0 $ lines ss)
      a1 = sum $ (\p -> if p then 1 else 0) <$> snd (mapAccumL (\cnt line -> (cnt + 1, tree 1 cnt line)) 0 $ lines ss)
      a2 = sum $ (\p -> if p then 1 else 0) <$> snd (mapAccumL (\cnt line -> (cnt + 1, tree 3 cnt line)) 0 $ lines ss)
      a3 = sum $ (\p -> if p then 1 else 0) <$> snd (mapAccumL (\cnt line -> (cnt + 1, tree 5 cnt line)) 0 $ lines ss)
      a4 = sum $ (\p -> if p then 1 else 0) <$> snd (mapAccumL (\cnt line -> (cnt + 1, tree 7 cnt line)) 0 $ lines ss)
      a5 = sum $ (\p -> if p then 1 else 0) <$> snd (mapAccumL (\cnt line -> (cnt + 1, even cnt && tree 1 (cnt `div` 2) line)) 0 $ lines ss)
  -- print (a1,a2,a3,a4,a5)
  let b = a1 * a2 * a3 * a4 * a5
  putTextLn $ "result is " <> show (a, b)
  pure (a, b)

aoc2 :: IO (Int, Int)
aoc2 = do
  ss <- readFileText "data/aoc2.dat"
  let xxx x = ret
        where
          [y0, y1, pwd] = words x
          [mins, maxs] = T.splitOn "-" y0
          mini = fromMaybe 0 $ readMaybe . toString $ mins
          maxi = fromMaybe 0 $ readMaybe . toString $ maxs
          c = T.head y1
          ch = T.singleton $ T.head y1
          n = T.count ch pwd
          p1 = T.index pwd (mini + 1) == c
          p2 = T.index pwd (maxi + 1) == c
          p0 = n >= mini && n <= maxi
          ret = (mini, maxi, c, pwd, n)
      yyy x = ret
        where
          (mini, maxi, c, pwd, n) = xxx x
          p0 = n >= mini && n <= maxi
          l = T.length pwd
          p1 = (mini - 1 < l) && T.index pwd (mini - 1) == c
          p2 = (maxi - 1 < l) && T.index pwd (maxi - 1) == c
          ret = (p0, p1 `xor` p2)
  -- print $ map xxx $ take 5 $ lines ss
  let a = sum $ map ((\p0 -> if p0 then 1 else 0) . fst . yyy) $ lines ss
      b = sum $ map ((\p0 -> if p0 then 1 else 0) . snd . yyy) $ lines ss
  putTextLn $ "result is " <> show (a, b)
  pure (a, b)

aoc1 :: IO (Int, Int)
aoc1 = do
  -- response <- httpLBS "https://adventofcode.com/2020/day/1/input"
  -- putStrLn $ "status code: " ++ show (getResponseStatusCode response)
  -- L8.putStrLn $ getResponseBody response
  ss <- readFileText "data/aoc1.dat"
  let is :: [Int]
      is = mapMaybe (readMaybe . toString) $ words ss
  let as = [(i, j, i * j) | i <- is, j <- is, i + j == 2020]
      bs = [(i, j, k, i * j * k) | i <- is, j <- is, k <- is, i + j + k == 2020]
  -- print as
  -- print bs
  let Just (_, _, a) = viaNonEmpty head as
  let Just (_, _, _, b) = viaNonEmpty head bs
  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)
