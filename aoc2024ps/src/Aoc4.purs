module Aoc4 (aoc4) where

import Data.Array (any, drop, fold, foldl, head, init, length, range, sort, take, transpose, zipWith, unzip, mapMaybe)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (fromArray, foldr1, foldl1) as NEA
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Functor ((<$>))
import Data.Int (fromString)
import Data.Map (Map, empty, insertWith, lookup) as M
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust)
import Data.Ord (class Ord, abs, min)
import Data.Show (class Show)
import Data.String (drop, indexOf, length, split, splitAt) as S
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (lines, words)
-- indexOf :: Pattern -> String -> Maybe Int
-- lastIndexOf :: Pattern -> String -> Maybe Int
-- contains :: Pattern -> String -> Boolean
-- split :: Pattern -> String -> Array String
-- splitAt :: Int -> String -> { after :: String, before :: String }

import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Effect (Effect)
import Effect.Console (log, logShow)
import Effect.Exception (Error)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafePartial)
import Prelude (bind, const, discard, flip, map, pure, show, Unit, ($), (+), (-), (*), (>), (<), (&&), (||), (==), (/=), (<<<), (<*>), (<>))

slurp :: String -> Effect ( Array String )
slurp fn = unsafePartial $ fromJust <$> init <$> lines <$> readTextFile UTF8 fn

readMaybeInts :: String -> Array ( Maybe Int )
readMaybeInts l = map fromString $ S.split (Pattern " ") l

dropNth :: forall a. Array a -> Int -> Array a
dropNth xs n = take n xs <> drop (n + 1) xs

-- Tuples
fst :: forall a b. Tuple a b -> a
fst ( Tuple v _ ) = v
snd :: forall a b. Tuple a b -> b
snd ( Tuple _ v ) = v
bimap :: forall a b c d. (a -> b) -> (c -> d) -> Tuple a c -> Tuple b d
bimap f g (Tuple a b) = Tuple (f a) (g b)

-- breaking Strings
breakOnAll :: Pattern -> String -> Array (Tuple String String)
breakOnAll p s = unfoldr go 0
  where
    Pattern pp = p
    l = S.length pp
    go :: Int -> Maybe (Tuple ( Tuple String String) Int)
    go i = res
     where
      si = S.drop i s
      mi = S.indexOf p si
      res = case mi of
                Just ii -> Just ( Tuple (_splitConv <<< S.splitAt ( ii + i ) $ s) (ii + i + l))
                Nothing -> Nothing

_splitConv :: { before :: String, after :: String } -> Tuple String String
_splitConv {before : b, after : a} = Tuple b a

breakOn :: Pattern -> String -> Tuple String String
breakOn p s = res
  where
    mi = S.indexOf p s
    res = case mi of
              Just i -> _splitConv <<< S.splitAt i $ s
              Nothing -> Tuple s ""

print :: forall a. Show a => a -> Effect Unit
print = logShow

-------------------------------------------------------------------

parseLine :: String -> Int
parseLine l = n
 where
  n = n0 + n1
  n0 = length <<< breakOnAll (Pattern "XMAS" ) $ l
  n1 = length <<< breakOnAll (Pattern "SAMX" ) $ l


aoc4 :: Effect (Tuple Int Int)
aoc4 = do
  ls <- take 10 <$> slurp "data/aoc4.dat"
  let ml :: Maybe Int
      ml = S.length <$> head ls
  let ll = fromMaybe 0 $ S.length <$> head ls
      nl = length ls
      tl = nl * ll

  print [ll, nl, tl]


  let a = 0
  let b = 0

  pure $ Tuple a b
