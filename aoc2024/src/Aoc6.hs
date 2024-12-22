module Aoc6 (aoc6) where

import Data.List as L (and, break, length, nub, (!!))
import qualified Data.Map.Strict as M (empty, insert, insertWith, keys, lookup, toList)
import Data.Maybe (Maybe, catMaybes, fromJust, mapMaybe)
import qualified Data.Text as T (breakOn, breakOnAll, concat, drop, filter, findIndex, index, intercalate, length, lines, splitOn, transpose)

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
type Coor = (Int, Int)
data Dir = DUp | DRight | DDown | DLeft
dd :: Dir -> (Int, Int)
dd DUp = (0, -1)
dd DRight = (1, 0)
dd DDown = (0, 1)
dd DLeft = (-1, 0)
ddx :: Dir -> Int
ddx = fst . dd
ddy :: Dir -> Int
ddy = snd . dd

aoc6 :: IO (Int, Int)
aoc6 = do
  ls <- slurp "data/aoc6.dat" <&> drop 10
  let ll = T.length . fromJust . viaNonEmpty head $ ls
      nl = length ls
      tl = nl * ll

  let lup :: [Text] -> (Int, Int) -> Maybe Char
      lup ts (x, y)
        | y < 0 || y >= ll = Nothing
        | x < 0 || x >= nl = Nothing
        | otherwise = Just $ T.index (ts L.!! y) x
  let coords :: Int -> (Int, Int)
      coords i = (i `mod` ll, i `div` nl)
  let (x, y) = coords . fromJust . T.findIndex (== '^') . T.concat $ ls
  let dir0 :: Dir; dir0 = DUp
  let step ::
        ((Int, Int), Dir) ->
        Maybe ((Int, Int), ((Int, Int), Dir))
      step ((x0, y0), dir) = case lup ls (x0 + ddx dir, y0 + ddy dir) of
        Nothing -> Nothing
        Just '#' -> Just ((x0, y0), ((x0 + ddx dir', y0 + ddy dir'), dir'))
         where
          dir' = case dir of -- turn to the right
            DUp -> DRight
            DRight -> DDown
            DDown -> DLeft
            DLeft -> DUp
        _ -> Just ((x0, y0), ((x0 + ddx dir, y0 + ddy dir), dir))

  let trac :: [(Int, Int)]
      trac = unfoldr step ((x, y), dir0)

  let a = length . L.nub $ trac
  let b = 0
  pure (a + 1, b)
