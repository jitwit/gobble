{-# language OverloadedStrings, ImplicitParams, TemplateHaskell, LambdaCase #-}
{-# language TypeOperators, DataKinds, TypeApplications, ViewPatterns #-}
{-# language PatternSynonyms #-}

module Main where

import Control.Lens
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text (Text)
import Data.List (union,(\\))
import Data.Proxy
import qualified Data.Map as M
import Data.Map (Map)

import Control.Exception
import Control.Monad
import Control.Monad.State
import System.Process
import Data.Time.Clock

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM

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
new'board = do
  b:as <- gobbler
  t <- getCurrentTime
  return $ Board t b (M.fromList $ zip as (repeat 1))

type WordList = [Text]
type Name = Text
type Game = (Board,[Player])

data Board = Board
  { _creationTime :: UTCTime
  , _letters :: Text
  , _word'list :: Map Text Int }
data Player = Player
  { _connection :: Connection
  , _answers :: Map Text Int
  , _score :: Int }
data Gobble = Gobble
  { _board :: Board
  , _players :: Map Name Player }

makeLenses ''Board
makeLenses ''Player
makeLenses ''Gobble

score'word :: Text -> Int
score'word = ([0,0,0,1,1,2,3,5,11] !!) . min 8 . T.length

score'submissions :: [[Text]] -> [Int]
score'submissions = walk [] where
  score ys x xs = sum [ score'word w | w <- x \\ foldr union ys xs ]
  walk ys zs = case zs of
    x:[] -> [score ys x []]
    x:xs -> score ys x xs : walk (union x ys) xs

start'state :: IO Gobble
start'state = Gobble <$> new'board <*> pure M.empty

fetch'board :: (?gobble :: TVar Gobble, MonadIO m) => m Text
fetch'board = liftIO $ view (board.letters) <$> readTVarIO ?gobble

fresh'board :: (?gobble :: TVar Gobble, MonadIO m) => m ()
fresh'board = liftIO $ atomically . modifyTVar' ?gobble . set board =<< new'board

is'name'free :: (?gobble :: TVar Gobble, MonadIO m) => Name -> m Bool
is'name'free name = liftIO $
  not . isn't _Nothing . preview (players.ix name) <$> readTVarIO ?gobble

new'player :: (?gobble :: TVar Gobble, MonadIO m) => Name -> Connection -> m ()
new'player who conn = liftIO $ do
  atomically $ modifyTVar' ?gobble $ players . at who ?~ Player conn M.empty 0
  print $ who <> " joined the chat"

name'player :: (?gobble :: TVar Gobble, MonadIO m) => Connection -> m Name
name'player conn = liftIO $ do
  uname <- receiveData conn
  is'name'free uname >>= \case
    False -> do reply'simply conn "name-is-taken"
                name'player conn
    True  -> do board <- fetch'board
                let response = A.object [ "board" A..= board ]
                new'player uname conn
                sendTextData conn $ A.encode response
                pure uname

remove'player :: (?gobble :: TVar Gobble, MonadIO m) => Name -> m ()
remove'player who = liftIO $ atomically $ modifyTVar' ?gobble $
  players . at who .~ Nothing

reply'simply :: MonadIO m => Connection -> Text -> m ()
reply'simply conn msg = liftIO $ sendTextData conn (A.encode msg)

reply :: (?gobble::TVar Gobble, WebSocketsData a, MonadIO m) => Connection -> a -> m ()
reply conn = liftIO . sendTextData conn

reply'json :: (?gobble::TVar Gobble, MonadIO m) => Connection -> A.Value -> m ()
reply'json conn = reply conn . A.encode

send'json :: MonadIO m => A.Value -> Connection -> m ()
send'json obj conn = liftIO $ sendTextData conn (A.encode obj)

submit'word :: (?gobble::TVar Gobble, MonadIO m) => Name -> Text -> m Bool
submit'word who word = liftIO $ atomically $ do
  gob <- readTVar ?gobble
  case gob ^? board.word'list.ix word of
    Nothing -> pure False
    Just _ -> do
      writeTVar ?gobble (gob & players.ix who.answers.at word ?~ 1)
      pure True

handle'control :: (?gobble::TVar Gobble, MonadIO m) => Name -> Connection -> ControlMessage -> m ()
handle'control who conn = liftIO . \case
  Close{}  -> print (who <> " has left the chat") >> remove'player who
  Ping msg -> print (who <> " pinged")
  Pong msg -> print (who <> " pinged")

broadcast :: (?gobble::TVar Gobble, WebSocketsData a, MonadIO m) => a -> m ()
broadcast msg = liftIO $ readTVarIO ?gobble >>=
  mapMOf_ (players.folded.connection) (`sendTextData` msg)

request'wordlists :: (?gobble::TVar Gobble, MonadIO m) => m ()
request'wordlists = broadcast @Text "wordlist"

pattern Query cmd <- Text cmd _
pattern Word w <- Text (T.words.T.toUpper.T.pack.B.unpack -> "GOBBLE":w:[]) _

handle'data :: (?gobble::TVar Gobble, MonadIO m) => Name -> Connection -> DataMessage -> m ()
handle'data who conn = liftIO . \case
  Word w -> reply'json conn . A.Bool =<< submit'word who w
  Query "who-else" -> sendTextData conn . A.encode =<< get'players
  Query "words" -> sendTextData conn . A.encode =<< get'persons'words who
  Text msg _  -> print msg >> reply'simply conn "idk/text"
  Binary msg  -> print msg >> reply'simply conn "idk/bin"

get'players :: (?gobble :: TVar Gobble) => IO [Name]
get'players = M.keys . view players <$> readTVarIO ?gobble

get'persons'words :: (?gobble :: TVar Gobble) => Name -> IO [Text]
get'persons'words who = readTVarIO ?gobble <&>
  (^..players.ix who.answers.to M.keys.folded)

-- "ws backend"
-- talk'player :: (?gobble :: TVar Gobble, MonadIO m) => Name -> Connection -> m ()
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
      link ! H.rel "stylesheet" ! H.href "static/glibble.css"
      script ! H.src "static/gobble.js" $ ""
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
  let back = serve boggle'api boggle'server
  let ?gobble = gobble
   in run 8000 $ websocketsOr defaultConnectionOptions boggle back

