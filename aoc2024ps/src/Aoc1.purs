
module Aoc1 (aoc1) where

import Prelude ( bind, discard, pure, show, Unit, ($), (+), (-), (*) )
import Data.Array ( foldl, unzip, zipWith, mapMaybe, sort )
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Functor ((<$>))
import Data.Int ( fromString )
import Data.Map (Map, empty, insertWith, lookup) as M
import Data.Maybe ( Maybe(..), fromJust)
import Data.Ord (class Ord, abs)
import Data.String.Utils ( lines, words )
import Data.Tuple ( Tuple(..) )
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (Error)
import Node.FS.Sync (readTextFile)
import Node.Encoding ( Encoding(..) )

-- readInt :: Text -> Int -- crash if not an integer
-- readInt = fromJust . readMaybe . toString . T.filter (/= '+')

slurp :: String -> Effect ( Array String )
slurp fn = lines <$> readTextFile UTF8 fn

parseLine :: String -> Maybe (Tuple Int Int)
parseLine line = case words line of
  [x, y] -> do
    xInt <- fromString x
    yInt <- fromString y
    pure $ Tuple xInt yInt
  _ -> Nothing

findWithDefault :: forall k v. Ord k => v -> k -> M.Map k v -> v
findWithDefault def k m = case M.lookup k m of
        Just a -> a
        Nothing -> def

aoc1 :: Effect (Tuple Int Int)
aoc1 = do
  ls <- slurp "data/aoc1.dat"

  let ps = mapMaybe parseLine $ ls
      (Tuple fs ss) = unzip ps
      (Tuple sfs sss) = Tuple (sort fs) (sort ss)
      a = sum $ zipWith (\x y -> abs (x - y)) sfs sss

  let m :: M.Map Int Int
      m = foldl (\m' x -> M.insertWith (+) x 1 m') M.empty ss
      b = foldl (\cnt k -> cnt + k * findWithDefault 0 k m) 0 fs

  pure $ Tuple a b
