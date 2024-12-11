
module Aoc1 (aoc1) where

import Prelude ( bind )
import Data.Int ( fromString )
import Data.Maybe ( Maybe, fromJust)
import Data.String.Utils ( lines, words )
import Data.Tuple ( Tuple(..) )
import Data.Array ( unzip, zipWith )
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (Error)
import Node.FS.Async (readTextFile)
import Node.Encoding ( Encoding(..) )

-- readInt :: Text -> Int -- crash if not an integer
-- readInt = fromJust . readMaybe . toString . T.filter (/= '+')

readData :: String -> Effect String
readData = readTextFile UTF8 displayFile
displayFile :: Either Error String -> Effect Unit
displayFile (Right fileData) = log fileData
displayFile (Left err) = log $ show err

parseLine :: String -> Maybe (Tuple Int Int)
parseLine line = case words line of
  [x, y] -> do
    xInt <- fromString x
    yInt <- fromString y
    pure (Tuple xInt yInt)
  _ -> Nothing

aoc1 :: Effect (Tuple Int Int)
aoc1 = do
  fc <- readData "data/aoc1.dat"
  -- testHSlurp =<< readData "dat/tav-0.dat"
  print $ lines fc
  let ps = mapMaybe parseLine $ lines fc
  let (Tuple fs ss) = unzip ps
  let (Tuple sfs sss) = Tuple (sort fs) (sort ss)
  let a = sum $ zipWith (\x y -> abs (x - y)) sfs sss

  pure (Tuple a 0)
