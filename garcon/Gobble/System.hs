{-# language OverloadedStrings #-}

module Gobble.System where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import System.Directory
import System.Process
import System.Random.Shuffle
import Data.Time.Clock
import Data.Default

import Gobble.Dawggle
import qualified Gobble.Dawg as D
import Gobble.Render
import Gobble.Core

sys'gobble :: FilePath -> Int -> IO [T.Text]
sys'gobble gobbler n = do
  let cmd = proc gobbler ["-n",show n,"-dawg","/var/www/gobble/static/collins.fasl","-stdout"]
  drop 1 . T.lines . T.pack <$> readCreateProcess cmd ""

new'board :: D.Node -> H.HashMap T.Text T.Text -> T.Text -> IO Board
new'board d h b = do
  let bs = boggle'search d $ T.unpack b
  t <- getCurrentTime
  write'board b
  return $ Board t b $ M.fromList [ (w,h H.! w) | w <- T.pack <$> bs ]

refill'pool :: Gobble -> IO Gobble
refill'pool g = do
  bs <- sys'gobble (g^.gobbler'path) (200 - (g^.solution'pool & length))
  return $ g & solution'pool <>~ bs

start'state :: FilePath -> IO Gobble
start'state gobbler'path = do
  b0:bs <- retrieve'boards
  d <- fetch'dict
  col <- retrieve'dictionary
  b0 <- new'board d col b0
  pinous <- make'pinou'stream
  return $ Gobble (-1)
                  b0
                  mempty
                  mempty
                  def
                  (Chat mempty)
                  pinous
                  mempty
                  col
                  bs
                  gobbler'path
                  d

retrieve'boards :: IO [T.Text]
retrieve'boards = do
  (as,bs) <- splitAt 1000 . T.words  <$> T.readFile "static/boards.txt"
  T.writeFile "static/boards.txt" $ T.unwords bs
  return as

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
