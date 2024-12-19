module Aoc4 (aoc4) where

-- “class (Functor t, Foldable t) => Traversable (t :: * -> *) where
-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
-- sequenceA :: Applicative f => t (f a) -> f (t a)
-- mapM :: Monad m => (a -> m b) -> t a -> m (t b)
-- sequence :: Monad m => t (m a) -> m (t a)
-- {-# MINIMAL traverse | sequenceA #-}”

import Data.List as L (length, (!!))
import Data.Maybe (Maybe, catMaybes, fromJust, mapMaybe)
import qualified Data.Text as T (breakOn, breakOnAll, drop, filter, index, intercalate, length, lines, splitOn, transpose)

debug :: a -> Text -> a
debug a t = flip trace a (toString t)

slurp :: FilePath -> IO [Text]
slurp fn = T.lines . decodeUtf8 <$> readFileBS fn

readInt :: Text -> Int -- crash if not an integer
readInt = fromJust . readMaybe . toString . T.filter (/= '+')

readMaybeInt :: Text -> Maybe Int
readMaybeInt = readMaybe . toString . T.filter (/= '+')

readMaybeInts :: Text -> [Maybe Int]
readMaybeInts l = map readMaybeInt $ T.splitOn " " l

-------------------------------------------------------------------

parseLine :: Text -> Int
parseLine l = n
 where
  n = n0 + n1
  n0 = L.length . T.breakOnAll "XMAS" $ l
  n1 = L.length . T.breakOnAll "SAMX" $ l

aoc4 :: IO (Int, Int)
aoc4 = do
  ls <- slurp "data/aoc4.dat" <&> drop 10
  let vs = T.transpose ls
  let ll = T.length . fromJust . viaNonEmpty head $ ls
      nl = length ls
      tl = nl * ll

  -- print [ll, nl, tl]

  let lup :: [Text] -> (Int, Int) -> Maybe Char
      lup ts (r, c)
        | r < 0 || r >= ll = Nothing
        | c < 0 || c >= nl = Nothing
        | otherwise = Just $ T.index (ts L.!! r) c
  let dNext (r, c) = (r + 1, c + 1)
  let d4 :: [Text] -> Int -> Int -> Int
      d4 ts r c = sum [x (+) (+), x (+) (-), x (-) (+), x (-) (-)]
       where
        x f g
          | "XMAS" == catMaybes [lup ts (r, c), lup ts (r `f` 1, c `g` 1), lup ts (r `f` 2, c `g` 2), lup ts (r `f` 3, c `g` 3)] = 1
          | otherwise = 0

  let ds = [d4 ls i j | i <- [0 .. ll], j <- [0 .. nl]]

  let a = sum $ fmap parseLine ls <> fmap parseLine vs <> ds

  let d3 :: [Text] -> Int -> Int -> Int
      d3 ts r c
        | (x0 == "MAS" || x0 == "SAM")
            && (x1 == "MAS" || x1 == "SAM") =
            1
        | otherwise = 0
       where
        x0 = catMaybes [lup ts (r - 1, c - 1), lup ts (r, c), lup ts (r + 1, c + 1)]
        x1 = catMaybes [lup ts (r - 1, c + 1), lup ts (r, c), lup ts (r + 1, c - 1)]

  let xs = [d3 ls i j | i <- [0 .. ll], j <- [0 .. nl]]
  let b = sum xs
  pure (a, b)
