{-# language OverloadedStrings, ImplicitParams, TemplateHaskell, LambdaCase #-}
{-# language TypeOperators, DataKinds, TypeApplications, ViewPatterns #-}
{-# language PatternSynonyms, MultiWayIf #-}

module Main where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Lazy.UTF8 as B8
import Data.Bool
import Data.Char
import Data.Text (Text)
import Data.Proxy
import qualified Data.Map as M
import Unsafe.Coerce

import Control.Exception (finally)
import Control.Monad
import Control.Monad.State
import System.Environment
import System.Directory
import System.Random
import Data.Time.Clock

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM

import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Servant.HTML.Blaze
import Servant.Server.StaticFiles
import Servant.Server
import Servant.API
import Network.Wai.Handler.Warp
import qualified Data.Aeson as A

import Gobble.Core
import Gobble.Render

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
start'state = start <$> new'board where
  start b = Gobble b M.empty Ready (Chat M.empty) (-1)

fetch'board :: (?gobble :: TVar Gobble, MonadIO m) => m Board
fetch'board = liftIO $ view board <$> readTVarIO ?gobble

is'name'free :: (?gobble :: TVar Gobble, MonadIO m) => Name -> m Bool
is'name'free name = liftIO $ do
  check'1 <- not.isn't _Nothing.preview (players.ix name) <$> readTVarIO ?gobble
  return $ name /= "" && name /= "null" && check'1

new'player :: (?gobble :: TVar Gobble, MonadIO m) => Name -> Connection -> m ()
new'player who conn = liftIO $ do
  gob <- atomically $ stateTVar ?gobble $ dup .
    (players.at who ?~ Player conn M.empty 0 0 0)
  when (gob ^. game'phase & isn't _Ready) $ do
    reply'json conn $ tag'thing "board" $ html'of'board (gob^.board)
    reply'json conn $ A.object
      [ "time" A..= (gob^.board.creation'time.to (addUTCTime round'period))
      , "pause" A..= score'length
      , "round" A..= round'length ]
  add'tweet "GOBBLE" (who <> " joined the chat.")

name'player :: (?gobble :: TVar Gobble, MonadIO m) => Connection -> m Name
name'player conn = liftIO $ do
  uname <- receiveData conn
  is'name'free uname >>= \case
    False -> reply'json @Text conn "name-is-taken" >> name'player conn
    True  -> do T.putStrLn $ uname <> " joined the chat."
                new'player uname conn
                pure uname

remove'player :: (?gobble :: TVar Gobble, MonadIO m) => Name -> m ()
remove'player who = liftIO $ atomically $ modifyTVar' ?gobble $
  players.at who .~ Nothing

reply :: (?gobble :: TVar Gobble, WebSocketsData a, MonadIO m) => Connection -> a -> m ()
reply conn = liftIO . sendTextData conn

reply'json :: (?gobble :: TVar Gobble, A.ToJSON j, MonadIO m) => Connection -> j -> m ()
reply'json conn = reply conn . A.encode

submit'words :: (?gobble :: TVar Gobble, MonadIO m) => Name -> [Text] -> m ()
submit'words who words = liftIO $ atomically $ do
  let ok'word w = T.all isLetter w
  gob <- readTVar ?gobble
  when (gob ^. game'phase & isn't _Scoring) $ writeTVar ?gobble $
    gob & players.ix who.answers
        %~ flip (foldr (\w -> bool id (at w?~1) (ok'word w))) words

delete'word :: (?gobble :: TVar Gobble, MonadIO m) => Name -> Text -> m ()
delete'word who word = liftIO $ atomically $ do
  gob <- readTVar ?gobble
  when (gob ^. game'phase & isn't _Scoring) $ writeTVar ?gobble
    (gob & players.ix who.answers.at word .~ Nothing)

add'tweet :: (?gobble :: TVar Gobble, MonadIO m) => Name -> Text -> m ()
add'tweet who tweet = liftIO $ do
  now <- getCurrentTime
  atomically $ modifyTVar' ?gobble $
    (chat'room.messages.at now?~Chat'Message tweet who) .
    (chat'room.messages %~ \m -> M.drop (M.size m - 10) m)
  tweet'chat
  print (who,tweet)

tweet'chat :: (?gobble :: TVar Gobble, MonadIO m) => m ()
tweet'chat = liftIO $ tagged'broadcast "chirp" =<< render'chat <$> readTVarIO ?gobble

handle'control :: (?gobble :: TVar Gobble, MonadIO m) => Name -> Connection -> ControlMessage -> m ()
handle'control who conn = liftIO . \case
  Close{}  -> remove'player who >> add'tweet "GOBBLE" (who <> " left the chat.")
  Ping msg -> T.putStrLn (who <> " pinged")
  Pong msg -> T.putStrLn (who <> " ponged")

broadcast :: (?gobble :: TVar Gobble, WebSocketsData a, MonadIO m) => a -> m ()
broadcast msg = liftIO $ readTVarIO ?gobble >>=
  mapMOf_ (players.folded.connection) (`sendTextData` msg)

broadcast'val :: (?gobble :: TVar Gobble, A.ToJSON a, MonadIO m) => a -> m ()
broadcast'val = broadcast . A.encode

tagged'broadcast :: (?gobble :: TVar Gobble, A.ToJSON a, MonadIO m) => Text -> a -> m ()
tagged'broadcast tag = broadcast'val . tag'thing tag

broadcast'clear :: (?gobble :: TVar Gobble, MonadIO m) => Text -> m ()
broadcast'clear what = tagged'broadcast what clear'html

pattern Query cmd <- Text cmd _
pattern Words ws <- Text (T.words.T.toUpper.T.pack.B.unpack -> "GOBBLE":ws) _
pattern Delete w <- Text (T.words.T.toUpper.T.pack.B.unpack -> "DOBBLE":w:[]) _
pattern Chirp msg <- Text (T.stripPrefix "chirp ".T.pack.B8.toString -> Just msg) _

handle'data :: (?gobble :: TVar Gobble, MonadIO m)
  => Name -> Connection -> DataMessage -> m ()
handle'data who conn = liftIO . \case
  Words ws -> submit'words who ws
  Delete w -> delete'word who w
  Chirp c -> add'tweet who c
  Query "who-else" -> reply'json conn =<< get'players
  Query "words" -> send'words conn who
  Text msg _ -> print msg >> reply'json @Text conn "idk/text"
  Binary msg -> print msg >> reply'json @Text conn "idk/bin"

get'players :: (?gobble :: TVar Gobble) => IO [Name]
get'players = M.keys . view players <$> readTVarIO ?gobble

update'phase :: (?gobble :: TVar Gobble, MonadIO m) => m ()
update'phase = liftIO $ atomically $ modifyTVar' ?gobble switch'phase where
  switch'phase = game'phase %~ \case
    Scoring -> Boggled
    Boggled -> Scoring
    other   -> other

send'words :: (?gobble :: TVar Gobble, MonadIO m) => Connection -> Name -> m ()
send'words conn who = liftIO $ do
  gob <- readTVarIO ?gobble
  when (gob ^. game'phase & isn't _Scoring) $
    reply'json conn $ tag'thing "words" $ render'words who gob

score'round :: (?gobble :: TVar Gobble, MonadIO m) => m ()
score'round = liftIO $ do
  gob <- atomically $ stateTVar ?gobble (dup.score'submissions)
  putStrLn "Scored Round... "
  print $ game'log'view gob
  broadcast'clear "words"
  tagged'broadcast "pinou" . html'of'pinou =<< new'pinou
  broadcast'val $ A.object
    [ "solution" A..= render'solution gob
    , "scores"   A..= render'scores gob ]

new'pinou :: IO String
new'pinou = do
  let img'dir = "static/images/"
  imgs <- listDirectory img'dir
  j <- randomRIO (0,length imgs-1)
  return $ img'dir <> imgs !! j

reset'gobble :: (?gobble :: TVar Gobble, MonadIO m) => m ()
reset'gobble = liftIO $ do
  atomically $ modifyTVar' ?gobble $ (game'phase .~ Ready) . (current'round .~ (-1))
  putStrLn "ready!"

fresh'round :: (?gobble :: TVar Gobble, MonadIO m) => m ()
fresh'round = liftIO $ do
  b <- new'board
  gob <- (board .~ b) . (game'phase .~ Boggled) .
         (current'round %~ ((`mod`5).succ)) .
         (players.traversed.answers .~ M.empty) <$>
         readTVarIO ?gobble
  atomically $ writeTVar ?gobble gob
  putStrLn "New Round"
  tagged'broadcast "board" $ html'of'board b
  broadcast'clear "pinou"
  broadcast'val $ A.object
    [ "time"     A..= (b^.creation'time.to (addUTCTime round'period))
    , "pause"    A..= score'length
    , "round"    A..= round'length
    , "scores"   A..= clear'html
    , "solution" A..= clear'html ]

run'gobble :: (?gobble :: TVar Gobble, MonadIO m) => m ()
run'gobble = liftIO $ forever $ do
  gob <- readTVarIO ?gobble
  let gobble'dt t = diffUTCTime t $ gob ^. board.creation'time
      peeps = gob ^. players & isn't _Empty
      phase = gob ^. game'phase
  dt <- unsafeCoerce . gobble'dt <$> getCurrentTime
  case phase of
    Ready   -> when peeps $ fresh'round
    Boggled -> if | not peeps -> reset'gobble
                  | dt > (10^12)*round'length -> score'round
                  | otherwise -> mempty
    Scoring -> if | not peeps -> reset'gobble
                  | dt > (10^12)*overall'length -> fresh'round
                  | otherwise -> mempty
  case phase of
    Ready   -> threadDelay $ ready'length
    running -> threadDelay $ run'length

-- "ws backend"
boggle :: (?gobble :: TVar Gobble, MonadIO m) => PendingConnection -> m ()
boggle pend = liftIO $ do
  conn <- acceptRequest pend
  forkPingThread conn 30
  who <- name'player conn
  flip finally (remove'player who) $ forever $ receive conn >>= \case
    ControlMessage ctl -> handle'control who conn ctl
    DataMessage _ _ _ msg -> handle'data who conn msg

-- "frontend"
boggle'api :: Proxy BoggleAPI
boggle'api = Proxy

type BoggleAPI =
       Get '[HTML] GobblePage
  :<|> "static" :> Raw
  :<|> "boards" :> Get '[JSON] Int
  :<|> "help" :> "naked" :> Get '[PlainText] String

check'boards :: Handler Int
check'boards = liftIO $ length <$> listDirectory "boards/"

naked'state :: (?gobble :: TVar Gobble) => Handler String 
naked'state = liftIO $ do
  gob <- readTVarIO ?gobble
  return $ unlines [gob & game'log'view & show,"",gob ^. chat'room & show]

boggle'server :: (?gobble :: TVar Gobble) => Server BoggleAPI
boggle'server = pure GobblePage
  :<|> serveDirectoryWebApp "static"
  :<|> check'boards
  :<|> naked'state

launch'boggle :: Int -> IO ()
launch'boggle port = do
  putStrLn $ "Starting GOBBLE on port " <> show port
  gobble <- newTVarIO =<< start'state
  let ?gobble = gobble
   in do let back = serve boggle'api boggle'server
         bog'thread <- forkIO $ run port $
           websocketsOr defaultConnectionOptions boggle back
         run'gobble `finally` killThread bog'thread
         putStrLn $ "GOBBLE died"

main :: IO ()
main = map read <$> getArgs >>= \case
  [] -> launch'boggle 8080
  x:_ -> launch'boggle x
