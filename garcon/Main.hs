{-# language OverloadedStrings, ImplicitParams, TemplateHaskell, MultiWayIf, LambdaCase #-}
{-# language TypeOperators, DataKinds #-}

module Main where

import qualified Data.Text as T
import Data.Text (Text)
import Data.List (union,(\\))
import Control.Lens
import Data.Proxy

import Control.Monad
import Control.Monad.State

-- import Control.Exception
import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM

import System.Process
import GHC.IO.Handle

import Text.Blaze.Html5 as H hiding (map,main)

import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Servant.HTML.Blaze
import Servant.Server
import Servant.API
import Network.Wai.Handler.Warp


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

new'player :: (?gobble :: TVar Gobble, MonadIO m) => UName -> Connection -> m ()
new'player uname conn = liftIO $ atomically $ modifyTVar' ?gobble $
  players %~ cons (uname,conn,[])

register'player :: (?gobble :: TVar Gobble, MonadIO m) => Connection -> m ()
register'player conn = do
  uname <- liftIO $ receiveData conn
  is'uname'free uname >>= \case
    False -> register'player conn
    True  -> new'player uname conn

get'connection :: (?gobble :: TVar Gobble, MonadIO m) => UName -> m (Maybe Connection)
get'connection who = liftIO $ do
  ps <- _players <$> readTVarIO ?gobble
  return $ fst <$> uncons [ c | p@(n,c,ws) <- ps, n == who ]

boggle :: (?gobble :: TVar Gobble, MonadIO m) => PendingConnection -> m ()
boggle pend = do
  conn <- liftIO $ acceptRequest pend
  liftIO $ forkPingThread conn 30
  register'player conn

del'player :: (?gobble :: TVar Gobble, MonadIO m) => Player -> m ()
del'player = todo

boggleAPI :: Proxy BoggleAPI
boggleAPI = Proxy

data HomePage = HomePage

instance ToMarkup HomePage where
  toMarkup _ = html $ do H.head $ title "gobble"
                         H.h1 "hihi"
                         H.body $ p "hihi"

type BoggleAPI = Get '[HTML] HomePage -- :<|> "home" :> Raw

home'handler :: Handler HomePage
home'handler = pure HomePage

todo = error "todo"

main :: IO ()
main = do
  gobble <- newTVarIO =<< start'state
  print $ score'submissions [["cat","birds"],["dogwood","cat"],["church"]]
  let addr = "127.0.0.1"
      port = 8000
  let ?gobble = gobble
   in let bog = websocketsOr defaultConnectionOptions boggle (serve boggleAPI home'handler)
       in run port bog

-- time board state maintain websocket connections

-- loop:
--   new board
--   for each conn: boadcast new board
--   listen for new cons, respond with board state/time
--   on expire: ask for submissions
--   score submissions
--   report scores
--   loop
