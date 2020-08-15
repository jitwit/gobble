{-# language OverloadedStrings, ImplicitParams, TemplateHaskell, LambdaCase #-}
{-# language TypeOperators, DataKinds, TypeApplications, ViewPatterns #-}
{-# language PatternSynonyms, MultiWayIf #-}

module Main where

import Control.Lens
import Control.Lens.Extras
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
import Gobble.System
import Gobble.Render

fetch'board :: (?gobble :: TVar Gobble, MonadIO m) => m Board
fetch'board = liftIO $ view board <$> readTVarIO ?gobble

is'name'free :: (?gobble :: TVar Gobble, MonadIO m) => Name -> m Bool
is'name'free name = liftIO $ do
  check'1 <- not.isn't _Nothing.preview (players.ix name) <$> readTVarIO ?gobble
  return $ name /= "" && name /= "null" && check'1

new'player :: (?gobble :: TVar Gobble, MonadIO m) => Name -> Connection -> m ()
new'player who conn = liftIO $ do
  now <- liftIO getCurrentTime
  gob <- atomically $ stateTVar ?gobble $ dup .
    (players.at who ?~ Player M.empty 0 0 0 now Here) .
    (connections.at who ?~ conn)
  when (gob ^. game'phase & isn't _Ready) $ do
    reply'json conn $ tag'thing "board" $ html'of'board (gob^.board)
    reply'json conn $ A.object
      [ "time" A..= (gob^.board.creation'time.to (addUTCTime round'period))
      , "pause" A..= score'length
      , "round" A..= round'length ]
    when (gob ^. game'phase & is _Scoring) $
      reply'json conn $ A.object
      [ "pinou"    A..= (html'of'pinou $ gob^.pinou'stream._head)
      , "solution" A..= render'solution gob
      , "scores"   A..= render'scores gob ]
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
  (players.at who .~ Nothing) . (connections.at who .~ Nothing)

reply :: (?gobble :: TVar Gobble, WebSocketsData a, MonadIO m)
      => Connection -> a -> m ()
reply conn = liftIO . sendTextData conn

reply'json :: (?gobble :: TVar Gobble, A.ToJSON j, MonadIO m)
           => Connection -> j -> m ()
reply'json conn = reply conn . A.encode

wow'wow :: (?gobble :: TVar Gobble, MonadIO m) => Name -> Text -> m ()
wow'wow who word = liftIO $ do
  gob <- readTVarIO ?gobble
  when (Just word /= (gob^.gobble'likes.at who)) $ do
    atomically $ writeTVar ?gobble $ gob & gobble'likes.at who ?~ word
    add'tweet "GOBBLE" $ T.unwords [who,"likes",word]

submit'words :: (?gobble :: TVar Gobble, MonadIO m) => Name -> [Text] -> m ()
submit'words who words = liftIO $ do
  now <- liftIO getCurrentTime
  atomically $ do
    let ok'word w = T.all isLetter w
    gob <- readTVar ?gobble
    when (gob ^. game'phase & isn't _Scoring) $ writeTVar ?gobble $
      gob & players.ix who %~
          ((answers %~ flip (foldr (\w -> bool id (at w?~1) (ok'word w))) words) .
           (last'activity .~ now))

delete'word :: (?gobble :: TVar Gobble, MonadIO m) => Name -> Text -> m ()
delete'word who word = liftIO $ atomically $ do
  gob <- readTVar ?gobble
  when (gob ^. game'phase & isn't _Scoring) $ writeTVar ?gobble
    (gob & players.ix who.answers.at word .~ Nothing)

game'ongoing :: (?gobble :: TVar Gobble, MonadIO m) => m Bool
game'ongoing = liftIO $ readTVarIO ?gobble <&> is _Boggled . view game'phase

parse'chirp :: (?gobble :: TVar Gobble, MonadIO m) => Name -> Text -> m ()
parse'chirp who chirp = case T.words chirp of
  ["?help"] -> help'message
  ["?def",word] -> add'tweet who chirp >> game'ongoing >>= \case
    False -> definition'of'ws word
    True  -> add'tweet "GOBBLE" $ who <> " has been added to santa's naughty list"
  _ -> add'tweet who chirp

add'tweet :: (?gobble :: TVar Gobble, MonadIO m) => Name -> Text -> m ()
add'tweet who tweet = liftIO $ do
  now <- getCurrentTime
  atomically $ modifyTVar' ?gobble $
    (players.ix who.last'activity.~now) .
    (chat'room.messages.at now?~Chat'Message tweet who) .
    (chat'room.messages %~ \m -> M.drop (M.size m - 10) m)
  tweet'chat
  print (who,tweet)

tweet'chat :: (?gobble :: TVar Gobble, MonadIO m) => m ()
tweet'chat = liftIO $ tagged'broadcast "chirp" =<< render'chat <$>
  readTVarIO ?gobble

handle'control :: (?gobble :: TVar Gobble, MonadIO m)
               => Name -> Connection -> ControlMessage -> m ()
handle'control who conn = liftIO . \case
  Close{}  -> remove'player who >> add'tweet "GOBBLE" (who <> " left the chat.")
  Ping msg -> T.putStrLn (who <> " pinged")
  Pong msg -> T.putStrLn (who <> " ponged")

broadcast :: (?gobble :: TVar Gobble, WebSocketsData a, MonadIO m) => a -> m ()
broadcast msg = liftIO $ readTVarIO ?gobble >>=
  mapMOf_ (connections.folded) (`sendTextData` msg)

broadcast'val :: (?gobble :: TVar Gobble, A.ToJSON a, MonadIO m) => a -> m ()
broadcast'val = broadcast . A.encode

tagged'broadcast :: (?gobble :: TVar Gobble, A.ToJSON a, MonadIO m)
                 => Text -> a -> m ()
tagged'broadcast tag = broadcast'val . tag'thing tag

broadcast'clear :: (?gobble :: TVar Gobble, MonadIO m) => Text -> m ()
broadcast'clear what = tagged'broadcast what clear'html

pattern Query cmd <- Text cmd _
pattern Words ws <- Text (T.words.T.toUpper.T.pack.B8.toString -> "GOBBLE":ws) _
pattern Delete w <- Text (T.words.T.toUpper.T.pack.B8.toString -> "DOBBLE":w:[]) _
pattern Wow'wow w <- Text (T.words.T.toUpper.T.pack.B8.toString -> "WOBBLE":w:[]) _
pattern Chirp msg <- Text (T.stripPrefix "chirp ".T.pack.B8.toString -> Just msg) _

handle'data :: (?gobble :: TVar Gobble, MonadIO m)
            => Name -> Connection -> DataMessage -> m ()
handle'data who conn = liftIO . \case
  Words ws -> submit'words who ws
  Delete w -> delete'word who w
  Chirp c -> parse'chirp who c
  Wow'wow w -> wow'wow who w
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
  tagged'broadcast "pinou" $ html'of'pinou $ gob ^. pinou'stream._head
  broadcast'val $ A.object
    [ "solution" A..= render'solution gob
    , "scores"   A..= render'scores gob ]

reset'gobble :: (?gobble :: TVar Gobble, MonadIO m) => m ()
reset'gobble = liftIO $ do
  atomically $ modifyTVar' ?gobble $
    (game'phase .~ Ready) .
    (current'round .~ (-1)) .
    (chat'room .~ Chat mempty)
  putStrLn "ready!"

fresh'round :: (?gobble :: TVar Gobble, MonadIO m) => m ()
fresh'round = liftIO $ do
  b <- new'board
  gob <- (board .~ b) . (game'phase .~ Boggled) .
         (current'round %~ ((`mod`5).succ)) .
         (pinou'stream %~ tail) .
         (gobble'likes .~ mempty) .
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
  now <- getCurrentTime
  let gobble'dt t = diffUTCTime t $ gob ^. board.creation'time
      peeps = gob ^. players & isn't _Empty
      phase = gob ^. game'phase
      dt = unsafeCoerce $ gobble'dt now
  status'same <- atomically $ stateTVar ?gobble (update'activity now)
  unless status'same $ tweet'chat >> putStrLn "status changed for someone"
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

definition'of'ws :: (?gobble :: TVar Gobble, MonadIO m) => T.Text -> m ()
definition'of'ws = add'tweet "GOBBLE" . maybe "idk" id <=< definition'of

help'message :: (?gobble :: TVar Gobble, MonadIO m) => m ()
help'message = add'tweet "GOBBLE" helpmsg where
  helpmsg = "((\"?def\" look up word) (\"?help\" this message))"

definition'of :: (?gobble :: TVar Gobble, MonadIO m) => T.Text -> m (Maybe T.Text)
definition'of word = liftIO $ readTVarIO ?gobble <&> (^?english.ix (T.toUpper word))

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
  :<|> "define" :> Capture "word" Text :> Get '[PlainText] Text

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
  :<|> fmap (maybe "idk" id) . definition'of

launch'boggle :: Int -> FilePath -> IO ()
launch'boggle port gobble'path = do
  putStrLn "Initializing GOBBLE"
  gobble <- newTVarIO =<< start'state gobble'path
  putStrLn $ "Starting     GOBBLE on port " <> show port
  let ?gobble = gobble
   in do let back = serve boggle'api boggle'server
         bog'thread <- forkIO $ run port $
           websocketsOr defaultConnectionOptions boggle back
         run'gobble `finally` killThread bog'thread
         putStrLn $ "GOBBLE died"

main :: IO ()
main = getArgs >>= \case
  [] -> launch'boggle 8011 "/home/jo/code/gobble/cobble"
  ["-p",x,"-g",g] -> launch'boggle (read x) g
  _ -> error "bad args"
