
{-# LANGUAGE OverloadedStrings #-}

module AOC2021D11 ( aoc11 ) where

import qualified Data.Map.Strict as M
import qualified Relude.Unsafe as Unsafe
import qualified Data.Text as T

debug :: a -> Text -> a
debug a b = trace (toString b) a

readInt :: Text -> Int -- crash if not an integer
readInt = Unsafe.fromJust . readMaybe . toString . T.filter (/= '+')

type Os = Map Int Int

aoc11 :: IO (Int, Int)
aoc11 = do
  ss <- readFileText "data/aoc11.dat"
  let ls = lines ss
      ll = T.length . Unsafe.head $ ls
      nl = length ls
      tl = nl*ll
      os :: Os
      -- use position tl as flash counter
      os = M.insert tl 0 . M.fromList . zip [0::Int ..] . map (\c -> ord c - ord '0') . concatMap toString $ ls

  let step :: Os -> Os
      step m = stepf  . M.map (+1) $ m -- Achtung, this also incs our counter!
      stepf m = let f = energy tl m; mm = M.foldrWithKey flash m m; ff = energy tl mm
                 in if f == ff then mm else stepf mm

      energy :: Int -> Os -> Int
      energy i s = fromMaybe 999 . M.lookup i $ s
      -- inc :: Char -> Char; inc = chr . (+1) . ord
      neighbors :: Int -> [Int]; neighbors i = catMaybes [ ul, u, ur, l, r, dl, d, dr ] where
                    u = if i > (ll-1) then Just (i-ll) else Nothing
                    d = if i < tl-nl then Just (i+ll) else Nothing
                    l = if i `mod` ll /= 0 then Just (i-1) else Nothing
                    r = if i `mod` ll /= (ll - 1) then Just (i+1) else Nothing
                    ul = if i > (ll-1) && i `mod` ll /= 0 then Just (i-ll-1) else Nothing
                    ur = if i > (ll-1) && i `mod` ll /= (ll - 1) then Just (i-ll+1) else Nothing
                    dl = if i < tl-nl && i `mod` ll /= 0 then Just (i+ll-1) else Nothing
                    dr = if i < tl-nl && i `mod` ll /= (ll - 1) then Just (i+ll+1) else Nothing

      flash :: Int -> Int -> Os -> Os
      flash i e m
        | i > (tl - 1) = m -- ignore, we store stuff here
        | e > 9 = M.adjust (+1) tl
                . M.insert i 0
                . foldr (M.adjust (\ ee -> if ee == 0 then ee else (+1) ee)) m
                $ neighbors i
        | otherwise = m

  let a = (energy tl . Unsafe.last . take (100 + 1) . iterate step $ os) - 100

  let step' m = let f0 = energy tl m in M.adjust (\f -> f-f0) tl . step $ m
  let b = length . takeWhile (/= 100) . map (\m -> energy tl m - 1) . iterate step' $ os

  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)
