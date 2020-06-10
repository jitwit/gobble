
module Gobble.System where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import System.Directory
import System.Random
import System.Random.Shuffle
import Data.Time.Clock

import Gobble.Render
import Gobble.Core

gobbler :: IO (T.Text, [T.Text])
gobbler = do
  let board'dir = "boards/"
  boards <- listDirectory board'dir
  board <- (boards!!) <$> randomRIO (0,length boards - 1)
  wds <- T.readFile (board'dir <> board)
  removeFile (board'dir <> board)
  return (T.pack board,T.lines wds)

new'board :: IO Board
new'board = do
  (b,as) <- gobbler
  t <- getCurrentTime
  write'board b
  return $ Board t b $ M.fromList [ (w,T.unwords def) | w:def <- map T.words as ]

start'state :: IO Gobble
start'state = do
  b0 <- new'board
  pinous <- make'pinou'stream
  return $ Gobble (-1) b0 mempty Ready (Chat mempty) pinous mempty

make'pinou'stream :: IO [FilePath]
make'pinou'stream =
  let img'dir = "static/images/"
   in do pinous <- shuffleM =<< listDirectory img'dir
         return [ img'dir <> pinou | pinou <- cycle pinous ]
