{-# language OverloadedStrings, ImplicitParams, TemplateHaskell, LambdaCase #-}
{-# language TypeOperators, DataKinds, TypeApplications, ViewPatterns #-}
{-# language PatternSynonyms, MultiWayIf #-}

module Main where

import Control.Lens
import Control.Lens.Extras
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.UTF8 as B8
import qualified Data.HashMap.Strict as H
import Data.Bool
import Data.Char
import Data.Text (Text)
import Data.Proxy
import qualified Data.Map as M
import Unsafe.Coerce

import qualified Control.Exception as E
import Control.Monad
import Control.Monad.State
import System.Environment
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
import Gobble.Dawggle

fetch'board :: (?gobble :: TVar Gobble, MonadIO m) => m Board
fetch'board = liftIO $ view board <$> readTVarIO ?gobble

is'name'ok :: (?gobble :: TVar Gobble, MonadIO m) => Name -> m Name'Check
is'name'ok name = liftIO $ do
  check'1 <- isn't _Nothing.preview (players.ix name) <$> readTVarIO ?gobble
  if | 12 < T.length name -> pure Name'Too'Long
     | name == "" || name == "null" -> pure Name'Is'Empty
     | check'1 -> pure Name'Taken
     | otherwise -> pure Name'OK

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
  is'name'ok uname >>= \case
    Name'Taken -> reply'json @Text conn "name-is-taken" >> name'player conn
    Name'Is'Empty -> reply'json @Text conn "name-is-empty" >> name'player conn
    Name'Too'Long -> reply'json @Text conn "name-too-long" >> name'player conn
    Name'OK  -> do T.putStrLn $ uname <> " joined the chat."
                   new'player uname conn
                   pure uname

remove'player :: (?gobble :: TVar Gobble, MonadIO m) => Text -> Name -> m ()
remove'player func who = liftIO $ do
  atomically $ modifyTVar' ?gobble $
    (players.at who .~ Nothing) . (connections.at who .~ Nothing)
  add'tweet "GOBBLE" (who <> " left the chat.")

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

handle'chirp :: (?gobble :: TVar Gobble, MonadIO m) => Name -> Chirp -> m ()
handle'chirp who = \case
  Help'Me -> help'message
  Who's'Gotten word -> liftIO $ do
    add'tweet who $ "?who " <> word
    g <- readTVarIO ?gobble
    ps <- past'solvers g (T.toUpper word)
    let msg = case ps of
          [] -> "..."
          ws -> T.intercalate ", " ps
    add'tweet "GOBBLE" msg
  Define word -> game'ongoing >>= \case
    False -> do add'tweet who $ "?def " <> word -- hmmmmmmm
                definition'of'ws word
    True  -> do add'tweet who $ "?def " <> (T.map (const '*') word)
                add'tweet "GOBBLE" $
                  who <> " has been added to santa's naughty list"
  Chirp chirp -> add'tweet who chirp

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

ws'handle :: (?gobble :: TVar Gobble, MonadIO m)
            => Name -> Connection -> DataMessage -> m ()
ws'handle who conn d'm = liftIO $ case parse'ws'message d'm of
  Status'Message Who'Query -> reply'json conn =<< get'players
  Status'Message Word'List'Query -> send'words conn who
  Submit'Message ws -> submit'words who ws
  Delete'Message w -> delete'word who w
  Like'Message w -> wow'wow who w
  Chirp'Message w -> print w >> handle'chirp who w
  IDK'Message idk -> reply'json @Text conn "idk"

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

send'preview :: (?gobble :: TVar Gobble, MonadIO m) => m ()
send'preview = liftIO $ do
  gob <- readTVarIO ?gobble
  when (gob ^. game'phase & isn't _Scoring) $
    tagged'broadcast "solution" $ render'preview gob

score'round :: (?gobble :: TVar Gobble, MonadIO m) => m ()
score'round = liftIO $ do
  gob <- atomically $ stateTVar ?gobble (dup.score'submissions)
  putStrLn "Scored round... "
  broadcast'clear "words"
  tagged'broadcast "pinou" $ html'of'pinou $ gob ^. pinou'stream._head
  broadcast'val $ A.object
    [ "solution" A..= render'solution gob
    , "scores"   A..= render'scores gob ]
  record'round gob
  putStrLn "Added round to DB..."
  atomically $ modifyTVar ?gobble update'previously'seen
  putStrLn "Recorded round..."

reset'gobble :: (?gobble :: TVar Gobble, MonadIO m) => m ()
reset'gobble = liftIO $ do
  atomically $ modifyTVar' ?gobble $
    (game'phase .~ Ready) .
    (current'round .~ (-1)) .
    (chat'room .~ Chat mempty)
  putStrLn "ready!"

fresh'round :: (?gobble :: TVar Gobble, MonadIO m) => m ()
fresh'round = liftIO $ do
  gob <- readTVarIO ?gobble
  b <- new'board (gob^.gobble'dawg) (gob^.english) (gob^.gobble'big'words)
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
  unless status'same $ tweet'chat
  case phase of
    Ready   -> when peeps fresh'round
    Boggled -> if | not peeps -> reset'gobble
                  | dt > (10^12)*round'length -> score'round
                  | otherwise -> mempty
    Scoring -> if | not peeps -> reset'gobble
                  | dt > (10^12)*overall'length -> fresh'round
                  | otherwise -> mempty
  case phase of
    Ready   -> threadDelay $ ready'length
    running -> threadDelay $ run'length

preview'loop :: (?gobble :: TVar Gobble, MonadIO m) => m ()
preview'loop = liftIO $ forever $ threadDelay (1000*1000*5) >> send'preview

definition'of'ws :: (?gobble :: TVar Gobble, MonadIO m) => T.Text -> m ()
definition'of'ws = add'tweet "GOBBLE" . maybe "idk" id <=< definition'of

help'message :: (?gobble :: TVar Gobble, MonadIO m) => m ()
help'message = add'tweet "GOBBLE" helpmsg where
  helpmsg = "((\"?def\" look up word) (\"?help\" this message))"

definition'of :: (?gobble :: TVar Gobble, MonadIO m) => T.Text -> m (Maybe T.Text)
definition'of word = liftIO $ readTVarIO ?gobble <&>
  (^?english.ix (T.toUpper word).definition)

http'boggle :: (?gobble :: TVar Gobble, MonadIO m) => T.Text -> m [T.Text]
http'boggle board = liftIO $ do
  dict <- view gobble'dawg <$> readTVarIO ?gobble
  return $ T.pack <$> boggle'search dict (T.unpack board)

-- "ws backend"
boggle :: (?gobble :: TVar Gobble, MonadIO m) => PendingConnection -> m ()
boggle pend = liftIO $ do
  conn <- acceptRequest pend
  who <- name'player conn
  withPingThread conn 30 (pure ()) $
    flip E.finally (remove'player "boggle" who) $ forever $
    receiveDataMessage conn >>= ws'handle who conn

-- "frontend"
boggle'api :: Proxy BoggleAPI
boggle'api = Proxy

type BoggleAPI =
  -- home page
       Get '[HTML] GobblePage
  -- serve static stuff
  :<|> "static" :> Raw
  -- view all history
  :<|> "previously" :> Get '[HTML] All'History'Page
  -- debug view
  :<|> "help" :> "naked" :> Get '[PlainText] String
  -- endpoint to query dictionary
  :<|> "define" :> Capture "word" Text :> Get '[PlainText] Text
  -- endpoint to solve board
  :<|> "dawggle" :> Capture "board" Text :> Get '[JSON] [Text]
  -- endpoint to reorder image stream
  :<|> "pinous" :> Get '[PlainText] Text
  -- danger
  :<|> "fix" :> "name" :> Capture "from" Text :> Capture "to" Text :> Get '[JSON] ()

naked'state :: (?gobble :: TVar Gobble) => Handler String 
naked'state = liftIO $ do
  gob <- readTVarIO ?gobble
  return $ unlines [gob & game'log'view & show,"",gob ^. chat'room & show]

boggle'server :: (?gobble :: TVar Gobble) => Server BoggleAPI
boggle'server = pure GobblePage
  :<|> serveDirectoryWebApp "static"
  :<|> all'history'page
  :<|> naked'state
  :<|> fmap (maybe "idk" id) . definition'of
  :<|> http'boggle
  :<|> refresh'pinous
  :<|> fix'username'for

fix'username'for :: (?gobble :: TVar Gobble, MonadIO m) => Text -> Text -> m ()
fix'username'for a b = liftIO $ do
  g <- readTVarIO ?gobble
  query'db g (run'update'solution'name a b)

all'history'page :: (?gobble :: TVar Gobble, MonadIO m) => m All'History'Page
all'history'page = liftIO $
  All'History'Page <$> (fetch'all'solutions =<< readTVarIO ?gobble)

debug'seen :: (?gobble :: TVar Gobble, MonadIO m) => m [Text]
debug'seen = liftIO $ do
  gob <- readTVarIO ?gobble
  return $ [ w | (w,Boggle'Word True _) <- gob^.english&H.toList ]

debug'seen'1 :: (?gobble :: TVar Gobble, MonadIO m) => Text -> m Bool
debug'seen'1 w = liftIO $ do
  gob <- readTVarIO ?gobble
  return $ Just True == gob ^? english . ix w . been'seen

refresh'pinous :: (?gobble :: TVar Gobble, MonadIO m) => m T.Text
refresh'pinous = liftIO $ do
  ps <- make'pinou'stream
  atomically $ modifyTVar' ?gobble $ pinou'stream .~ ps
  return "OK"

launch'boggle :: Int -> IO ()
launch'boggle port = do
  putStrLn "Initializing GOBBLE"
  gobble <- newTVarIO =<< start'state
  putStrLn $ "Starting     GOBBLE on port " <> show port
  let ?gobble = gobble
   in do let back = serve boggle'api boggle'server
         bog'thread <- forkIO $ run port $
           websocketsOr defaultConnectionOptions boggle back
         preview'thread <- forkIO $ preview'loop
         E.finally run'gobble $ do
           killThread bog'thread
           killThread preview'thread
           putStrLn $ "GOBBLE died"

main :: IO ()
main = getArgs >>= \case
  [] -> launch'boggle 8011
  ["-p",x] -> launch'boggle (read x)
  args -> error $ "idk what to do with: " <> unwords args
