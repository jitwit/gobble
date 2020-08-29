{-# language LambdaCase #-}

module Gobble.Dawggle ( boggle'search, fetch'dict, roll55 ) where

import Gobble.Dawg
import System.Directory
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import System.Random.Shuffle
import Control.Monad
import Data.List
import Data.Tree
import Data.Maybe

type Board = U.Vector Char
type Graph = V.Vector [Int]

board'of'string :: String -> Board
board'of'string = U.fromList

graph'of'board :: Board -> Graph
graph'of'board b = V.generate (U.length b) adj where
  n = floor $ sqrt $ fromIntegral $ U.length b
  adj j = [ (n*x') + y' | (dx,dy) <- (,) <$> [-1..1] <*> [-1..1]
          , let x' = x+dx; y' = y+dy, 0 <= min x' y', n > max x' y', (dx,dy)/=(0,0) ]
    where (x,y) = divMod j n

board'ref :: Board -> Int -> String
board'ref b j = case b U.! j of
  'Q' -> "QU"
  x   -> [x]

example = "YVUPESTAGOLEOWNV"

adjacent :: Board -> Graph -> Path -> [Path]
adjacent b g p@(Path j js n) =
  [ Path k (j:js) n' | k <- g V.! j \\ js, Just n' <- [lookupPrefix (board'ref b k) n] ]

start :: Node -> Board -> [Path]
start d b =
  [ Path i [] d' | i <- [0..U.length b - 1], let Just d' = lookupPrefix (board'ref b i) d ]

boggle'search :: Node -> String -> [String]
boggle'search d s =
  let b = board'of'string s; g = graph'of'board b
      look (Path j js n) = (guard $ endOfWord n) >> (Just $ board'ref b =<< reverse (j:js))
      forest = unfoldForest (\p -> (look p, adjacent b g p)) $ start d b
   in nub . sort . join . map catMaybes =<< (drop 2 $ transpose $ levels <$> forest)

data Path = Path { ix :: {-# unpack #-} !Int
                 , ixes :: [Int]
                 , dict :: {-# unpack #-} !Node
                 } deriving (Show)

dawggle = "static/dawggle.dawg"

fetch'dict :: IO Node
fetch'dict = doesFileExist dawggle >>= \case
  True -> fromFile dawggle
  False ->
    do toFile dawggle =<< fromAscList . map init . lines <$>
         readFile "/home/jo/code/gobble/cobble/share/collins.txt"
       fetch'dict

roll55 :: IO String
roll55 = map head <$> (shuffleM =<< traverse shuffleM dice55)

dice55 :: [String]
dice55 =
  ["QBZJXK","OOOTTU","OVWRGR","AAAFSR","AUMEEG"
   ,"HHLRDO","NDHTHO","LHNROD","AFAISR","YIFASR"
   ,"TELPCI","SSNSEU","RIYPHR","DORDLN","CCWNST"
   ,"TTOTEM","STCIEP","EANDNN","MNNEAG","UOTOWN"
   ,"AEAEEE","YIFPSR","EEEEMA","ITITIE","EITLIC"]
