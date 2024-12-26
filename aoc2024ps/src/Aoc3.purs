
module Aoc3 (aoc3) where

import Data.Array (any, drop, foldl, init, length, range, sort, take, zipWith, unzip, mapMaybe)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (fromArray, foldr1, foldl1) as NEA
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Functor ((<$>))
import Data.Int (fromString)
import Data.Map (Map, empty, insertWith, lookup) as M
import Data.Maybe (Maybe(..), fromJust, isJust)
import Data.Ord (class Ord, abs, min)
import Data.String (drop, indexOf, length, split, splitAt) as S
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (lines, words)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Effect (Effect)
import Effect.Console (log)
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

breakOnAll :: Pattern -> String -> Array ( {before :: String, after :: String } )
breakOnAll p s = unfoldr go 0
  where
    Pattern pp = p
    l = S.length pp
    go :: Int -> Maybe (Tuple {before :: String, after :: String} Int)
    go i = res
     where
      si = S.drop i s
      mi = S.indexOf p si
      res = case mi of
                Just ii -> Just ( Tuple (S.splitAt ( ii + i ) s) (ii + i + l))
                Nothing -> Nothing

breakOn :: Pattern -> String -> {before :: String, after :: String}
breakOn p s = res
  where
    mi = S.indexOf p s
    res = case mi of
              Just i -> S.splitAt i s
              Nothing -> {before : s, after : ""}

-------------------------------------------------------------------

terms :: String -> Array Int
terms = mapMaybe (\( Tuple a b ) -> (*) <$> a <*> b)
  <<< map xxx
  <<< breakOnAll (Pattern "mul(")
  where
    xx :: {before :: String, after :: String} -> Tuple (Maybe Int) (Maybe Int)
    xx { before : b, after : a } =
            Tuple ( fromString <<< S.drop 4 $ b)
                  ( fromString <<< S.drop 1 $ a)
    xxx :: {before :: String, after :: String}  -> Tuple (Maybe Int) (Maybe Int)
    xxx = xx
      <<< breakOn (Pattern ",")
      <<< _.before <<< ( breakOn (Pattern ")" ) )
      <<< _.after

-- indexOf :: Pattern -> String -> Maybe Int
-- lastIndexOf :: Pattern -> String -> Maybe Int
-- contains :: Pattern -> String -> Boolean
-- split :: Pattern -> String -> Array String
-- splitAt :: Int -> String -> { after :: String, before :: String }

parseLine :: String -> Int
parseLine = sum <<< terms

aoc3 :: Effect (Tuple Int Int)
aoc3 = do
  ls <- take 1 <$> slurp "data/aoc3.dat"

  log $ show ls
  log $ show <<< breakOnAll (Pattern "/") $ "a/b/c/"
  log $ show <<< breakOn (Pattern "/") $ "a/b/c/"
  log $ show <<< map (breakOnAll (Pattern "mul(")) $ ls

  let a = sum <<< map parseLine $ ls
      b = 0
  pure $ Tuple a b
