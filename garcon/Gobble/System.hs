{-# language OverloadedStrings #-}

module Gobble.System where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import System.Directory
import System.Process
import System.Random.Shuffle
import Data.Time.Clock
import Data.Default

import Gobble.Dawggle
import qualified Gobble.Dawg as D
import Gobble.Render
import Gobble.Core

new'board :: D.Node -> H.HashMap T.Text T.Text -> V.Vector String -> IO Board
new'board d h w = do
  b <- T.pack . snd <$> roll w
  let bs = boggle'search d $ T.unpack b
  t <- getCurrentTime
  write'board b
  return $ Board t b $ M.fromList [ (w,h H.! w) | w <- T.pack <$> bs ]

start'state :: IO Gobble
start'state = do
  (d,ws) <- fetch'dict
  col <- retrieve'dictionary
  b0 <- new'board d col ws
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
                  d
                  ws

retrieve'dictionary :: IO (H.HashMap T.Text T.Text)
retrieve'dictionary =
  let parse'line = fmap (T.drop 1) . T.breakOn "\t"
   in H.fromList . map parse'line . T.lines <$> T.readFile "static/definitions.txt"

make'pinou'stream :: IO [FilePath]
make'pinou'stream =
  let img'dir = "static/images/"
   in do ps1 <- shuffleM =<< listDirectory img'dir
         ps2 <- shuffleM ps1
         ps3 <- shuffleM ps2
         return [ img'dir <> pinou | pinou <- cycle $ ps1 <> ps2 <> ps3 ]
