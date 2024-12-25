module Aoc2 (aoc2) where

import Prelude (bind, discard, flip, map, pure, show, Unit, ($), (+), (-), (*), (>), (<), (&&), (||), (==), (/=), (<<<), (<*>))
import Partial.Unsafe (unsafePartial)

import Data.Array (drop, foldl, take, unzip, zipWith, mapMaybe, sort)
import Data.Array.NonEmpty (fromArray, foldr1, foldl1) as NEA
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Functor ((<$>))
import Data.Int (fromString)
import Data.Map (Map, empty, insertWith, lookup) as M
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Ord (class Ord, abs, min)
import Data.String (Pattern(..), split)
import Data.String.Utils (lines, words)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (Error)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

slurp :: String -> Effect ( Array String )
slurp fn =  lines <$> readTextFile UTF8 fn

readMaybeInts :: String -> Array ( Maybe Int )
readMaybeInts l = map fromString $ split (Pattern " ") l

isSafe :: Array ( Maybe Int ) -> Boolean
isSafe is = safe
 where
  safe = eith is
  -- return Nothing if neither a nor b is Just
  eith s = a  /= Nothing || b /= Nothing
   where
    ss = unsafePartial $ fromJust <<< NEA.fromArray $ s
    a = ascending ss
    b = descending ss
    ascending = NEA.foldl1 asc
    descending = NEA.foldr1 (flip asc)
    asc :: Maybe Int -> Maybe Int -> Maybe Int
    asc (Just l) (Just i) = if i > l && i - l < 4 then Just i else Nothing
    asc _ _ = Nothing

parseLine :: String -> Boolean
parseLine = isSafe <<< readMaybeInts


aoc2 :: Effect (Tuple Int Int)
aoc2 = do
  ls <- drop 6 <$> slurp "data/aoc2.dat"

  let a = foldl (\cnt i -> if i then cnt + 1 else cnt) 0 <<< map parseLine $ ls

  let b = 0
  pure $ Tuple a b
