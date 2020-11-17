{-# language LambdaCase, TypeApplications #-}

module Gobble.Dawggle
  ( boggle'search
  , fetch'dict
  , roll55
  , random'path
  , len'without'qu
  , del'qu
  , roll
  ) where

import Gobble.Dawg
import System.Directory
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import System.Random.Shuffle
import System.Random
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
   in nub . sort . join . map catMaybes =<< (transpose $ levels <$> forest)

data Path = Path { ix :: {-# unpack #-} !Int
                 , ixes :: [Int]
                 , dict :: {-# unpack #-} !Node
                 } deriving (Show)

dawggle = "static/dawggle.dawg"

fetch'dict :: IO (Node,V.Vector String)
fetch'dict = doesFileExist dawggle >>= \case
  True -> do trie <- fromFile dawggle
             return (trie, V.fromList
                      [ w | w <- toList trie, 10 < len'without'qu w ])
  False ->
    do toFile dawggle =<< fromAscList . map init . lines <$>
         readFile "/home/jo/code/gobble/cobble/share/collins.txt"
       fetch'dict

-- not checked, but n must obviously be in [0,16]
random'path :: Int -> IO [Int]
random'path n =
  let g = graph'of'board $ board'of'string $ take 16 $ cycle "gobble"
      lp i vs@(v:_)
        | i == n = pure vs
        | otherwise = shuffleM (g V.! v \\ vs) >>= \case
            [] -> lp 1 . (:[]) =<< randomRIO @Int (0,15)
            u:_ -> lp (1+i) $ u:vs
   in lp 1 . (:[]) =<< randomRIO @Int (0,15)

no'qu :: String -> (Char,Bool)
no'qu ('Q':'U':_) = ('U',False)
no'qu (x:y:_) = (y,True)

del'qu :: String -> String
del'qu s@(a:_) = a : (map fst $ filter snd $ map no'qu $ init $ init $ tails s)
del'qu s = s

len'without'qu :: String -> Int
len'without'qu = length . del'qu

roll :: V.Vector String -> IO (String,String)
roll words = do
  j <- randomRIO (0,pred $ V.length words)
  let s' = words V.! j
  let s = del'qu s'
  let n = length s
  p <- random'path n
  let js = [0..15] \\ p
  xs <- map (toEnum . (+65)) <$> replicateM (16-n) (randomRIO (0,25))
  let b = U.toList $ (U.replicate 16 'z') U.// zip (p <> js) (del'qu s <> xs)
  return (s',b)

pp'board :: String -> IO ()
pp'board s = case splitAt 4 s of
  (s,[]) -> putStrLn s
  (x,y) -> putStrLn x >> pp'board y

roll55 :: IO String
roll55 = map head <$> (shuffleM =<< traverse shuffleM dice55)

dice55 :: [String]
dice55 =
  ["QBZJXK","OOOTTU","OVWRGR","AAAFSR","AUMEEG"
   ,"HHLRDO","NDHTHO","LHNROD","AFAISR","YIFASR"
   ,"TELPCI","SSNSEU","RIYPHR","DORDLN","CCWNST"
   ,"TTOTEM","STCIEP","EANDNN","MNNEAG","UOTOWN"
   ,"AEAEEE","YIFPSR","EEEEMA","ITITIE","EITLIC"]
