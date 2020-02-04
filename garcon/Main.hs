{-# language OverloadedStrings, ImplicitParams, TemplateHaskell, MultiWayIf, LambdaCase #-}

module Main where

import Control.Monad
import Control.Monad.State

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM

import GHC.IO.Handle
import System.Process
import Network.Wai.Handler.WebSockets
import Network.WebSockets
import qualified Data.Text as T
import Data.Text (Text)
import Data.List
import Control.Lens

gobbler :: IO [T.Text]
gobbler = map T.pack . lines <$> readCreateProcess cmd "" where
  cmd = (shell "./gobbler.ss") { cwd = Just ".." }

new'board :: IO Board
new'board = do { b:as <- gobbler; return (b,as) }

type WordList = [Text]
type UName = Text
type Board = (Text,WordList)
type Game = (Board,[Player])
type Player = (UName,Connection,WordList)

data Gobble = Gobble { _board :: Board, _players :: [Player] }

makeClassy ''Gobble

score'word :: Text -> Int
score'word = ([0,0,0,1,1,2,3,5,11] !!) . min 8 . T.length

score'submissions :: [[Text]] -> [Int]
score'submissions = walk [] where
  score ys x xs = sum [ score'word w | w <- x \\ foldr union ys xs ]
  walk ys zs = case zs of
    x:[] -> [score ys x []]
    x:xs -> score ys x xs : walk (union x ys) xs

start'state :: IO Gobble
start'state = Gobble <$> new'board <*> pure []

fetch'board :: (?gobble :: TVar Gobble, MonadIO m) => m Text
fetch'board = liftIO $ fst . _board <$> readTVarIO ?gobble

fresh'board :: (?gobble :: TVar Gobble, MonadIO m) => m ()
fresh'board = liftIO $ atomically . modifyTVar' ?gobble . (board .~) =<< new'board

is'uname'free :: (?gobble :: TVar Gobble, MonadIO m) => UName -> m Bool
is'uname'free uname = liftIO $
  allOf (players.folded._1) (/=uname) <$> readTVarIO ?gobble

boggle :: (?gobble :: TVar Gobble) => PendingConnection -> IO ()
boggle pend = do
  conn <- acceptRequest pend
  forkPingThread conn 30
  uname <- receiveData conn
  is'uname'free uname >>= \case
    False -> todo
    True -> todo

del'player :: (?gobble :: TVar Gobble, MonadIO m) => Player -> m ()
del'player = todo

todo = error "todo"

main :: IO ()
main = do
  gob <- newTVarIO =<< start'state
  print $ score'submissions [["cat","birds"],["dogwood","cat"],["church"]]
  let ?gobble = gob in print =<< fetch'board

  

-- time board state maintain websocket connections

-- loop:
--   new board
--   for each conn: boadcast new board
--   listen for new cons, respond with board state/time
--   on expire: ask for submissions
--   score submissions
--   report scores
--   loop
