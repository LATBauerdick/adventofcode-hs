{-# LANGUAGE OverloadedStrings #-}

module AOC2021D14 ( aoc14 ) where

import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Relude.Unsafe as Unsafe
import qualified Data.Text as T

debug :: a -> Text -> a
debug a b = trace (toString b) a

readInt :: Text -> Int -- crash if not an integer
readInt = Unsafe.fromJust . readMaybe . toString . T.filter (/= '+')

aoc14 :: IO (Int, Int)
aoc14 = do
  ss <- readFileText "data/aoc14.dat"
  let template = Unsafe.head . lines $ ss
  let segs :: [(Text, Text)]; segs = parseLn . words <$> (drop 2 . lines $ ss)
      parseLn [p0, _, p1] = (p0,p1)
      parseLn _ = error "bad input from data/aoc5.dat"
  let segm :: Map (Char, Char) Text
      segm = M.fromList . map (\(p0,p1) -> ((T.head p0, T.last p0), T.snoc p1 (T.last p0) ) ) $ segs

  let step :: Text -> Text
      step ps = foldl' (\t p -> t <> Unsafe.fromJust (M.lookup p segm)) (T.singleton $ T.head ps) $ T.zip ps (T.drop 1 ps)

  let poly10 =  Unsafe.last . take 11 . iterate step $ template
  let cnts = map (\c -> T.count (T.singleton c) poly10)  . L.nub . toString $ poly10
  let a =  L.maximum cnts - L.minimum cnts

  let segt :: Map Text (Text, Text)
      segt =  M.fromList . map (\(p0,p1) -> (p0, (T.cons (T.head p0) p1 , T.snoc p1 (T.last p0)))) $ segs
  let cntm :: Map Text Int
      cntm = M.fromList . zip (M.keys segt) $ repeat 0

  let step0 :: Map Text Int
      step0 = foldr (M.adjust (+1) . (\(p0,p1) -> T.snoc (T.singleton p0) p1)) cntm
            $ T.zip template (T.drop 1 template)
  let step' :: Map Text Int ->  Map Text Int
      step' m = M.foldrWithKey doStep m m
      doStep :: Text -> Int -> Map Text Int -> Map Text Int
      doStep k i m = let (p0,p1) = Unsafe.fromJust (M.lookup k segt)
                      in  if i > 0
                             then M.adjust (\ii -> ii-i) k . M.adjust (+i) p1 . M.adjust (+i) p0 $ m `debug` ("xxx " <> k <> p0 <> p1)
                             else m

  let poly40 =  Unsafe.last . take (40+1) . iterate step' $ step0
  let cnts' = M.elems . foldr (\(k,i) m -> M.insertWith (+) (T.head k) i m) (M.singleton (T.last template) 1) . M.toList $ poly40

  let b =  L.maximum cnts' - L.minimum cnts'

  putTextLn $ "result is " <> show a <> " and " <> show b
  pure (a, b)
