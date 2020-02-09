{-# language OverloadedStrings, ImplicitParams, TemplateHaskell, LambdaCase #-}
{-# language TypeOperators, DataKinds, TypeApplications, ViewPatterns #-}
{-# language PatternSynonyms, MultiWayIf #-}

module Main where

import Control.Lens
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text (Text)
import Data.List (union,(\\),sortBy)
import Data.Function
import Data.Proxy
import qualified Data.Map as M
import Data.Map (Map)
import Unsafe.Coerce

import Control.Exception
import Control.Monad
import Control.Monad.State
import System.Process
import Data.Time.Clock

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM

import Text.Blaze.Html5 as H hiding (map,main,head)
import qualified Text.Blaze.Html5 as H (head)
import Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5.Attributes as H hiding (form)

import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Servant.HTML.Blaze
import Servant.Server.StaticFiles
import Servant.Server
import Servant.API
import Network.Wai.Handler.Warp
import qualified Data.Aeson as A

type Name = Text

data Phase = Boggled | Scoring deriving (Eq)
data Board = Board
  { _creationTime :: UTCTime
  , _letters :: Text
  , _word'list :: Map Text Text }
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

(.&) = M.intersection
(.-) = M.difference

gobbler :: IO [T.Text]
gobbler = map T.pack . lines <$> readCreateProcess cmd "" where
  cmd = (shell "scheme --script gobbler.ss 5x5") { cwd = Just ".." }

new'board :: IO Board
new'board = do
  b:as <- gobbler
  let ws = head . T.words <$> as
  t <- getCurrentTime
  return $ Board t b $ M.fromList [ (w,T.unwords def) | w:def <- map T.words as ]

start'state :: IO Gobble
start'state = start <$> new'board where
  start b = Gobble b M.empty Scoring 0

fetch'board :: (?gobble :: TVar Gobble, MonadIO m) => m Board
fetch'board = liftIO $ view board <$> readTVarIO ?gobble

is'name'free :: (?gobble :: TVar Gobble, MonadIO m) => Name -> m Bool
is'name'free name = liftIO $
  not . isn't _Nothing . preview (players.ix name) <$> readTVarIO ?gobble

new'player :: (?gobble :: TVar Gobble, MonadIO m) => Name -> Connection -> m ()
new'player who conn = liftIO $ do
  gob <- (players.at who ?~ Player conn M.empty 0) <$> readTVarIO ?gobble
  atomically $ writeTVar ?gobble gob
  reply'json conn $ tag'thing "board" $ renderHtml $ html'of'board (gob^.board)
  reply'json conn $ A.object
    [ "time" A..= (gob^.board.creationTime.to (addUTCTime round'period))
    , "pause" A..= score'length
    , "round" A..= round'length ]
  when (gob ^. game'phase & isn't _Scoring) $ do
    let peeps = renderHtml $ do
          h3 "who's here?"
          ul $ mapM_ (li.text) (gob^.players.to M.keys)
    broadcast'val $ tag'thing "peeps" peeps

name'player :: (?gobble :: TVar Gobble, MonadIO m) => Connection -> m Name
name'player conn = liftIO $ do
  uname <- receiveData conn
  is'name'free uname >>= \case
    False -> reply'simply conn "name-is-taken" >> name'player conn
    True  -> new'player uname conn >> pure uname

remove'player :: (?gobble :: TVar Gobble, MonadIO m) => Name -> m ()
remove'player who = liftIO $ do
  gob <- (players . at who .~ Nothing) <$> readTVarIO ?gobble
  atomically $ writeTVar ?gobble gob
  when (gob ^. game'phase & isn't _Scoring) $ do
    let peeps = renderHtml $ do
          h3 "who's here?"
          ul $ mapM_ (li.text) (gob^.players.to M.keys)
    broadcast'val $ tag'thing "peeps" peeps

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

pattern Query cmd <- Text cmd _
pattern Word w <- Text (T.words.T.toUpper.T.pack.B.unpack -> "GOBBLE":w:[]) _
pattern Delete w <- Text (T.words.T.toUpper.T.pack.B.unpack -> "DOBBLE":w:[]) _

handle'data :: (?gobble :: TVar Gobble, MonadIO m) => Name -> Connection -> DataMessage -> m ()
handle'data who conn = liftIO . \case
  Word w -> submit'word who w
  Delete w -> delete'word who w
  Query "who-else" -> reply'json conn =<< get'players
  Query "words" -> send'words conn who -- =<< get'persons'words who
  Text msg _ -> print msg >> reply'simply conn "idk/text"
  Binary msg -> print msg >> reply'simply conn "idk/bin"

get'players :: (?gobble :: TVar Gobble) => IO [Name]
get'players = M.keys . view players <$> readTVarIO ?gobble

update'phase :: (?gobble :: TVar Gobble, MonadIO m) => m ()
update'phase = liftIO $ atomically $ modifyTVar' ?gobble switch'phase where
  switch'phase = game'phase %~ \case
    Scoring -> Boggled
    Boggled -> Scoring

send'words :: (?gobble :: TVar Gobble, MonadIO m) => Connection -> Name -> m ()
send'words conn who = liftIO $ do
  gob <- readTVarIO ?gobble
  when (gob ^. game'phase & isn't _Scoring) $
    reply'json conn $ tag'thing "words" $ renderHtml $
    mapM_ (li.text) $ gob^.players.ix who.answers.to M.keys

score'submissions :: Gobble -> (Map Text Int, Gobble)
score'submissions gob = (all'subs, gob') where
  gob' = gob & players.traversed %~ scr & game'phase .~ Scoring
  solution = gob ^. board.word'list
  score'word = ([0,0,0,1,1,2,3,5,11] !!) . min 8 . T.length
  all'subs = gob ^.. players.traversed.answers & M.unionsWith (+)
  scr (Player conn sol scr) = Player conn sol (sum pts - sum npts) where
    pts = [ score'word word | (word,1) <- M.toList (all'subs .& sol .& solution) ]
    npts = [ score'word word | (word,1) <- M.toList (all'subs .& (sol .- solution)) ]

dec'len :: [Text] -> [Text]
dec'len = sortBy (flip compare `on` T.length)

score'round :: (?gobble :: TVar Gobble, MonadIO m) => m ()
score'round = liftIO $ do
  (all'subs,gob) <- score'submissions <$> readTVarIO ?gobble
  atomically $ writeTVar ?gobble gob
  let sol = gob ^. board.word'list
      results = renderHtml $ do
        h3 "scores"
        ul $ mapM_ id [ li $ text $ who <> " got " <> T.pack (show scr)
                      | (who, Player _ _ scr) <- M.toList (gob ^. players) ]
  broadcast'val $ tag'thing "peeps" results
  forM_ (gob^.players&M.toList) $ \(who,Player c a s) -> do
    let report = renderHtml $ do
          h4 "mistakes"
          ul $ mapM_ (li.text) (dec'len $ M.keys $ a.-sol)
          h4 "valid words"
          ul $ mapM_ (li.text) (dec'len $ M.keys $ a.&sol)
          h4 "common words"
          ul $ mapM_ (li.text) $ dec'len
            [ w | (w,n) <- M.toList (all'subs.&a), n > 1 ]
          h4 "missed words" -- make these better. definitions/sort out types
          ul $ mapM_ (li.text) (dec'len $ M.keys $ sol.-a)
    reply'json c $ tag'thing "words" report
    
threadDelayS :: Int -> IO ()
threadDelayS = threadDelay . (*10^6)

round'length, score'length :: Int
round'length = 90
score'length = 30
round'period :: NominalDiffTime
round'period = unsafeCoerce $ secondsToDiffTime $
  fromIntegral $ round'length + score'length

html'of'board :: Board -> Html
html'of'board b = table $ forM_ (b^.letters.to (T.chunksOf n)) $
  \row -> tr $ mapM_ (td.text.tt) $ T.unpack row where
    tt 'Q' = "Qu"
    tt x = T.singleton x
    ls = b^.letters
    n = floor $ sqrt $ fromIntegral $ T.length ls

fresh'round :: (?gobble :: TVar Gobble, MonadIO m) => m ()
fresh'round = liftIO $ do
  b <- new'board
  atomically $ modifyTVar' ?gobble $
    (board .~ b) . (game'phase .~ Boggled) .
    (players.traversed.answers .~ M.empty)
  broadcast'val $ tag'thing "board" $ renderHtml $ html'of'board b
  broadcast'val $ A.object
    [ "time" A..= (b^.creationTime.to (addUTCTime round'period))
    , "pause" A..= score'length
    , "round" A..= round'length ]

run'round :: (?gobble :: TVar Gobble, MonadIO m) => m ()
run'round = liftIO $ do
  fresh'round
  threadDelayS round'length
  score'round
  threadDelayS score'length

-- "ws backend"
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
          H.div ! H.id "timer" $ ""
          H.canvas ! H.id "hourglass" ! H.height "100" ! H.width "100" $ ""
          H.form ! H.id "mush" $ do
            H.input ! H.type_ "text" ! H.id "scratch"
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
   in do timer'thread <- forkIO $ forever run'round
         let bog = run 8000 $ websocketsOr defaultConnectionOptions boggle back
         bog `finally` killThread timer'thread
