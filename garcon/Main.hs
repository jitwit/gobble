{-# language OverloadedStrings, ImplicitParams, TemplateHaskell, LambdaCase #-}
{-# language TypeOperators, DataKinds, TypeApplications, ViewPatterns #-}
{-# language PatternSynonyms, MultiWayIf #-}

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
import qualified Text.Blaze.Html5.Attributes as H hiding (form)

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

type Name = Text

data Phase = Boggled | Scoring deriving (Eq)
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
  , _players :: Map Name Player
  , _game'phase :: Phase
  , _round :: Int }

makePrisms ''Phase
makeLenses ''Board
makeLenses ''Player
makeLenses ''Gobble

(.&) :: (Ord k, Ord a) => Map k a -> Map k a -> Map k a
(.&) = M.intersection
(.-) = M.difference

score'submissions :: Gobble -> Gobble
score'submissions gob = gob & players.traversed %~ scr where
  solution = gob ^. board.word'list
  score'word = ([0,0,0,1,1,2,3,5,11] !!) . min 8 . T.length
  all'subs = gob ^.. players.traversed.answers & M.unionsWith (+)
  scr (Player conn sol scr) = Player conn M.empty (sum pts - sum npts) where
    pts = [ score'word word | (word,1) <- M.toList (all'subs .& sol .& solution) ]
    npts = [ score'word word | (word,1) <- M.toList (all'subs .& (sol .- solution)) ]

start'state :: IO Gobble
start'state = start <$> new'board where
  start b = Gobble b M.empty Scoring 0

fetch'board :: (?gobble :: TVar Gobble, MonadIO m) => m Text
fetch'board = liftIO $ view (board.letters) <$> readTVarIO ?gobble

fresh'board :: (?gobble :: TVar Gobble, MonadIO m) => m ()
fresh'board = liftIO $ do
  b <- new'board
  atomically $ modifyTVar' ?gobble $
    (board .~ b) . (game'phase .~ Boggled)

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

reply :: (?gobble :: TVar Gobble, WebSocketsData a, MonadIO m) => Connection -> a -> m ()
reply conn = liftIO . sendTextData conn

reply'json :: (?gobble :: TVar Gobble, A.ToJSON j, MonadIO m) => Connection -> j -> m ()
reply'json conn = reply conn . A.encode

send'json :: MonadIO m => A.Value -> Connection -> m ()
send'json obj conn = liftIO $ sendTextData conn (A.encode obj)

tag'thing :: (A.ToJSON v) => Text -> v -> A.Value
tag'thing tag val = A.object [ tag A..= val ]

submit'word :: (?gobble :: TVar Gobble, MonadIO m) => Name -> Text -> m ()
submit'word who word = liftIO $ atomically $ do
  gob <- readTVar ?gobble
  when (gob ^. game'phase & isn't _Scoring) $ writeTVar ?gobble
    (gob & players.ix who.answers.at word ?~ 1)

delete'word :: (?gobble :: TVar Gobble, MonadIO m) => Name -> Text -> m ()
delete'word who word = liftIO $ atomically $ do
  gob <- readTVar ?gobble
  when (gob ^. game'phase & isn't _Scoring) $ writeTVar ?gobble
    (gob & players.ix who.answers.at word .~ Nothing)

handle'control :: (?gobble :: TVar Gobble, MonadIO m) => Name -> Connection -> ControlMessage -> m ()
handle'control who conn = liftIO . \case
  Close{}  -> print (who <> " has left the chat") >> remove'player who
  Ping msg -> print (who <> " pinged")
  Pong msg -> print (who <> " pinged")

broadcast :: (?gobble :: TVar Gobble, WebSocketsData a, MonadIO m) => a -> m ()
broadcast msg = liftIO $ readTVarIO ?gobble >>=
  mapMOf_ (players.folded.connection) (`sendTextData` msg)

broadcast'val :: (?gobble :: TVar Gobble, A.ToJSON a, MonadIO m) => a -> m ()
broadcast'val = broadcast . A.encode

request'wordlists :: (?gobble :: TVar Gobble, MonadIO m) => m ()
request'wordlists = broadcast @Text "wordlist"

pattern Query cmd <- Text cmd _
pattern Word w <- Text (T.words.T.toUpper.T.pack.B.unpack -> "GOBBLE":w:[]) _
pattern Delete w <- Text (T.words.T.toUpper.T.pack.B.unpack -> "DOBBLE":w:[]) _

handle'data :: (?gobble :: TVar Gobble, MonadIO m) => Name -> Connection -> DataMessage -> m ()
handle'data who conn = liftIO . \case
  Word w -> reply'json conn =<< submit'word who w
  Delete w -> reply'json conn =<< delete'word who w
  Query "who-else" -> reply'json conn =<< get'players
  Query "words" -> send'words conn who -- =<< get'persons'words who
  Text msg _ -> print msg >> reply'simply conn "idk/text"
  Binary msg -> print msg >> reply'simply conn "idk/bin"

get'players :: (?gobble :: TVar Gobble) => IO [Name]
get'players = M.keys . view players <$> readTVarIO ?gobble

send'words :: (?gobble :: TVar Gobble, MonadIO m) => Connection -> Name -> m ()
send'words conn who = liftIO (readTVarIO ?gobble) >>=
  reply'json conn . tag'thing "words" .
  toListOf (players.ix who.answers.to M.keys.folded)

threadDelayS :: Int -> IO ()
threadDelayS = threadDelay . (*10^6)

round'length, score'length :: Int
round'length = 90
score'length = 10

update'phase :: (?gobble :: TVar Gobble, MonadIO m) => m ()
update'phase = liftIO $ atomically $ modifyTVar' ?gobble switch'phase where
  switch'phase = game'phase %~ \case
    Scoring -> Boggled
    Boggled -> Scoring

score'round :: (?gobble :: TVar Gobble, MonadIO m) => m ()
score'round = liftIO $ do
  gob <- readTVarIO ?gobble
  let gob' = score'submissions gob & game'phase .~ Scoring
  atomically $ writeTVar ?gobble gob'

broadcast'scores :: (?gobble :: TVar Gobble, MonadIO m) => m ()
broadcast'scores = liftIO $ do
  gob <- readTVarIO ?gobble
  broadcast'val $ tag'thing "scores" $ A.object
    [ who A..= scr | (who, Player _ _ scr) <- M.toList (gob ^. players) ]

run'round :: (?gobble :: TVar Gobble, MonadIO m) => m ()
run'round = liftIO $ do
  fresh'board
  broadcast'val $ tag'thing @Text "round" "boggling"
  broadcast'val . tag'thing "board" =<< fetch'board
  threadDelayS round'length
  
  broadcast'val $ tag'thing @Text "round" "scoring"
  score'round
  broadcast'scores
  threadDelayS score'length

timer :: (?gobble :: TVar Gobble, MonadIO m) => m ()
timer = forever run'round

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
      script ! H.src "static/jquery-3.4.1.slim.js" $ ""
      script ! H.src "static/gobble.js" $ ""
    H.body $ do
      H.h1 "BOGGLE BITCH"
      H.div ! H.id "boggle" $ do
        H.div ! H.id "viz" $ do
          H.div ! H.id "gobble" $ ""
          H.div ! H.id "scores" $ ""
        H.div ! H.id "words" $ do
          H.form ! H.id "mush" $ do
            H.input
              ! H.type_ "text"
              ! H.id "scratch"
            H.input ! H.type_ "submit" ! H.value "mush!"
          H.ul ! H.id "submissions" $ ""

type BoggleAPI = Get '[HTML] GobblePage :<|> "static" :> Raw

boggle'server :: Server BoggleAPI
boggle'server = pure GobblePage :<|> serveDirectoryWebApp "static"

todo = error "todo"

main :: IO ()
main = do
  gobble <- newTVarIO =<< start'state
  let back = serve boggle'api boggle'server
  let ?gobble = gobble
   in do timer'thread <- forkIO timer
         let bog = run 8000 $ websocketsOr defaultConnectionOptions boggle back
         bog `finally` killThread timer'thread
