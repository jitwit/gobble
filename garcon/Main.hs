{-# language OverloadedStrings, ImplicitParams, TemplateHaskell, LambdaCase #-}
{-# language TypeOperators, DataKinds, TypeApplications, ViewPatterns #-}
{-# language PatternSynonyms #-}

module Main where

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text (Text)
import Data.List (union,(\\))
import Control.Lens
import Data.Proxy

import Control.Monad
import Control.Monad.State

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM

import System.Process

import Text.Blaze.Html5 as H hiding (map,main)
import qualified Text.Blaze.Html5.Attributes as H

import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Servant.HTML.Blaze
import Servant.Server.StaticFiles
import Servant.Server
import Servant.API
import Network.Wai.Handler.Warp

import qualified Data.Aeson as A

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
fresh'board = liftIO $ atomically . modifyTVar' ?gobble . set board =<< new'board

is'uname'free :: (?gobble :: TVar Gobble, MonadIO m) => UName -> m Bool
is'uname'free uname = liftIO $
  allOf (players.folded._1) (/=uname) <$> readTVarIO ?gobble

new'player :: (?gobble :: TVar Gobble, MonadIO m) => UName -> Connection -> m ()
new'player uname conn = liftIO $ do
  atomically $ modifyTVar' ?gobble $ players %~ cons (uname,conn,[])
  print $ uname <> " joined the chat"

name'player :: (?gobble :: TVar Gobble, MonadIO m) => Connection -> m UName
name'player conn = liftIO $ do
  uname <- receiveData conn
  is'uname'free uname >>= \case
    False -> do reply'simply conn "name-is-taken"
                name'player conn
    True  -> do board <- fetch'board
                let response = A.object [ "board" A..= board ]
                new'player uname conn
                sendTextData conn $ A.encode response
                pure uname

remove'player :: (?gobble :: TVar Gobble, MonadIO m) => UName -> m ()
remove'player who = liftIO $ atomically $ modifyTVar' ?gobble $
  players %~ filter ((/=who) . view _1)

reply'simply :: MonadIO m => Connection -> Text -> m ()
reply'simply conn msg = liftIO $ sendTextData conn (A.encode msg)

send'json :: MonadIO m => A.Value -> Connection -> m ()
send'json obj conn = liftIO $ sendTextData conn (A.encode obj)

-- submit'wordlist ::
submit'wordlist :: (?gobble::TVar Gobble, MonadIO m) => UName -> [Text] -> m ()
submit'wordlist who ws = liftIO $ do
  let aux p@(plr,c,_) | plr == who = (plr,c,ws)
                      | otherwise = p
  print (who,ws)
  atomically $ modifyTVar' ?gobble $ players %~ map aux

handle'control :: (?gobble::TVar Gobble, MonadIO m) => UName -> Connection -> ControlMessage -> m ()
handle'control who conn = liftIO . \case
  Close{}  -> print (who <> " has left the chat") >> remove'player who
  Ping msg -> print (who <> " pinged")
  Pong msg -> print (who <> " pinged")

broadcast :: (?gobble::TVar Gobble, MonadIO m) => A.Value -> m ()
broadcast obj = liftIO $ do
  gobble <- readTVarIO ?gobble
  gobble & mapMOf_ (players . folded . _2) (send'json obj)

request'wordlists :: (?gobble::TVar Gobble, MonadIO m) => m ()
request'wordlists = broadcast (A.String "wordlist")

pattern Who <- Text "who" _
pattern WordList ws <- Text (T.words . T.pack . B.unpack -> "words":ws) _

handle'data :: (?gobble::TVar Gobble, MonadIO m) => UName -> Connection -> DataMessage -> m ()
handle'data who conn = liftIO . \case
  WordList ws -> submit'wordlist who ws >> reply'simply conn "merci!"
  Who -> sendTextData conn . A.encode =<< get'players
  Text msg _  -> print msg >> reply'simply conn "idk"
  Binary msg  -> print msg >> reply'simply conn "idk"

get'players :: (?gobble :: TVar Gobble) => IO [UName]
get'players = (^..players.folded._1) <$> readTVarIO ?gobble
      
get'connection :: (?gobble :: TVar Gobble, MonadIO m) => UName -> m (Maybe Connection)
get'connection who = liftIO $ do
  ps <- _players <$> readTVarIO ?gobble
  return $ fst <$> uncons [ c | p@(n,c,ws) <- ps, n == who ]

-- "ws backend"
-- talk'player :: (?gobble :: TVar Gobble, MonadIO m) => UName -> Connection -> m ()
-- talk'player who conn = todo
boggle :: (?gobble :: TVar Gobble, MonadIO m) => PendingConnection -> m ()
boggle pend = liftIO $ do
  conn <- acceptRequest pend
  forkPingThread conn 30
  who <- name'player conn
  forever $ receive conn >>= \case
    ControlMessage ctl -> handle'control who conn ctl
    DataMessage _ _ _ msg -> handle'data who conn msg

-- "frontend"
boggle'api :: Proxy BoggleAPI
boggle'api = Proxy

data GobblePage = GobblePage

instance ToMarkup GobblePage where
  toMarkup _ = html $ do
    H.head $ do
      title "gobble"
      script ! H.src "static/gobble.js" $ "blah"
    H.body $ do
      H.h1 "BOGGLE BITCH"
      H.div ! H.id "gobble" $ ""

type BoggleAPI = Get '[HTML] GobblePage :<|> "static" :> Raw

boggle'server :: Server BoggleAPI
boggle'server = pure GobblePage :<|> serveDirectoryWebApp "static"

todo = error "todo"

main :: IO ()
main = do
  gobble <- newTVarIO =<< start'state
  let ?gobble = gobble
   in let back = serve boggle'api boggle'server
       in run 8000 $ websocketsOr defaultConnectionOptions boggle back

