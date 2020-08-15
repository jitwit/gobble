{-# language OverloadedStrings #-}

module Gobble.System where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import System.Directory
import System.Process
import System.IO
import System.Random
import System.Random.Shuffle
import Data.Time.Clock
import Data.Default

import Gobble.Render
import Gobble.Core

summon'gobbler :: FilePath -> IO Gobbler
summon'gobbler gobbler'path = do
  i <- openFile "gobble-in" ReadWriteMode
  o <- openFile "gobble-out" ReadWriteMode
  let g = RawCommand "scheme" ["--script","gobbler.ss","-loop"]
      s = (shell "gobbler -loop") { std_in = UseHandle i
                                  , std_out = UseHandle o
                                  , cwd = Just gobbler'path }
  withCreateProcess s $ \_ _ _ p -> do
    return $ Gobbler i o p

gobbler'fs :: IO (T.Text, [T.Text])
gobbler'fs = do
  let board'dir = "boards/"
  boards <- listDirectory board'dir
  board <- (boards!!) <$> randomRIO (0,length boards - 1)
  wds <- T.readFile (board'dir <> board)
  removeFile (board'dir <> board)
  return (T.pack board,T.lines wds)

new'board :: IO Board
new'board = do
  (b,as) <- gobbler'fs
  t <- getCurrentTime
  write'board b
  return $ Board t b $ M.fromList [ (w,T.unwords def) | w:def <- map T.words as ]

start'state :: FilePath -> IO Gobble
start'state gobble'path = do
  b0 <- new'board
  pinous <- make'pinou'stream
  col <- retrieve'dictionary
  gob <- summon'gobbler gobble'path
  return $ Gobble (-1) b0 mempty mempty def (Chat mempty) pinous mempty col gob

retrieve'dictionary :: IO (H.HashMap T.Text T.Text)
retrieve'dictionary =
  let parse'line = fmap (T.drop 1) . T.breakOn "\t"
   in H.fromList . map parse'line . T.lines <$> T.readFile "static/definitions.txt"

-- | nb. we specifically want images in static so they can be
-- served. this is in contrast to boards which absolutely must not be
-- visible, not that anyone in their right mind would bother combing
-- through them to gain an edge...
make'pinou'stream :: IO [FilePath]
make'pinou'stream =
  let img'dir = "static/images/"
   in do ps1 <- shuffleM =<< listDirectory img'dir
         ps2 <- shuffleM ps1
         ps3 <- shuffleM ps2
         return [ img'dir <> pinou | pinou <- cycle $ ps1 <> ps2 <> ps3 ]
