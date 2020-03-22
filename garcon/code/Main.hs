{-# language OverloadedStrings, ImplicitParams, TemplateHaskell, LambdaCase #-}
{-# language TypeOperators, DataKinds, TypeApplications, ViewPatterns #-}
{-# language PatternSynonyms, MultiWayIf #-}

module Main where

import Control.Lens
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text (Text)
import Data.List (union,(\\),sortBy,intercalate)
import Data.Function
import Data.Proxy
import qualified Data.Map as M
import Data.Map (Map)

import Control.Exception (finally)
import Control.Monad
import Control.Monad.State
import System.Environment
import System.Process
import System.Directory
import Data.Time.Clock
import Data.Hashable

import Control.Concurrent
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM

import Text.Blaze.Html5 as H hiding (map,main,head,style)
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

import Gobble.Core
import Gobble.Outils
import Gobble.Render

gobbler :: IO [T.Text]
gobbler = do
  let brds = "boards/"
  board <- (brds <>) . head <$> listDirectory brds
  contents <- T.lines <$> T.readFile board
  removeFile board
  return contents

new'board :: IO Board
new'board = do
  b:as <- gobbler
  t <- getCurrentTime
  write'board b
  return $ Board t b $ M.fromList [ (w,T.unwords def) | w:def <- map T.words as ]

start'state :: IO Gobble
start'state = start <$> new'board where
  start b = Gobble b M.empty Scoring (Chat M.empty) 0

fetch'board :: (?gobble :: TVar Gobble, MonadIO m) => m Board
fetch'board = liftIO $ view board <$> readTVarIO ?gobble

is'name'free :: (?gobble :: TVar Gobble, MonadIO m) => Name -> m Bool
is'name'free name = liftIO $
  (&&name/="").not.isn't _Nothing.preview (players.ix name) <$> readTVarIO ?gobble

new'player :: (?gobble :: TVar Gobble, MonadIO m) => Name -> Connection -> m ()
new'player who conn = liftIO $ do
  gob <- (players.at who ?~ Player conn M.empty 0) <$> readTVarIO ?gobble
  atomically $ writeTVar ?gobble gob
  let rnd = gob ^. current'round
  reply'json conn $ tag'thing "board" $ renderHtml $ html'of'board (gob^.board)
  reply'json conn $ A.object
    [ "time" A..= (gob^.board.creationTime.to (addUTCTime round'period))
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
remove'player who = liftIO $ do
  gob <- (players . at who .~ Nothing) <$> readTVarIO ?gobble
  atomically $ writeTVar ?gobble gob
  add'tweet "GOBBLE" (who <> " left the chat.")

reply :: (?gobble :: TVar Gobble, WebSocketsData a, MonadIO m) => Connection -> a -> m ()
reply conn = liftIO . sendTextData conn

reply'json :: (?gobble :: TVar Gobble, A.ToJSON j, MonadIO m) => Connection -> j -> m ()
reply'json conn = reply conn . A.encode

send'json :: MonadIO m => A.Value -> Connection -> m ()
send'json obj conn = liftIO $ sendTextData conn (A.encode obj)

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

add'tweet :: (?gobble :: TVar Gobble, MonadIO m) => Name -> Text -> m ()
add'tweet who tweet = liftIO $ do
  now <- getCurrentTime
  atomically $ modifyTVar' ?gobble $
    (chat'room.messages.at now ?~ Chat'Message tweet who) .
    (chat'room.messages %~ \m -> M.drop (M.size m - 10) m)
  tweet'chat

tweet'chat :: (?gobble :: TVar Gobble, MonadIO m) => m ()
tweet'chat = liftIO $ do
  gob <- readTVarIO ?gobble
  let whos'here = H.text $ T.intercalate ", " (gob^.players.to M.keys)
  broadcast'val $ tag'thing "chirp" $ renderHtml $ table $ do -- ! H.style "width:300px" $ do
    thead whos'here
    forM_ (gob^..chat'room.messages.folded) $ \(Chat'Message tweet author) ->
      tr $ do td $ text author
              td $ text tweet -- ! H.style "text-align:right" $ text tweet

handle'control :: (?gobble :: TVar Gobble, MonadIO m) => Name -> Connection -> ControlMessage -> m ()
handle'control who conn = liftIO . \case
  Close{}  -> T.putStrLn (who <> " has left the chat") >> remove'player who
  Ping msg -> T.putStrLn (who <> " pinged")
  Pong msg -> T.putStrLn (who <> " ponged")

broadcast :: (?gobble :: TVar Gobble, WebSocketsData a, MonadIO m) => a -> m ()
broadcast msg = liftIO $ readTVarIO ?gobble >>=
  mapMOf_ (players.folded.connection) (`sendTextData` msg)

broadcast'val :: (?gobble :: TVar Gobble, A.ToJSON a, MonadIO m) => a -> m ()
broadcast'val = broadcast . A.encode

pattern Query cmd <- Text cmd _
pattern Word w <- Text (T.words.T.toUpper.T.pack.B.unpack -> "GOBBLE":w:[]) _
pattern Delete w <- Text (T.words.T.toUpper.T.pack.B.unpack -> "DOBBLE":w:[]) _
pattern Chirp msg <- Text (T.stripPrefix "chirp ".T.pack.B.unpack -> Just msg) _

handle'data :: (?gobble :: TVar Gobble, MonadIO m)
  => Name -> Connection -> DataMessage -> m ()
handle'data who conn = liftIO . \case
  Word w -> submit'word who w
  Delete w -> delete'word who w
  Chirp c -> add'tweet who c
  Query "who-else" -> reply'json conn =<< get'players
  Query "words" -> send'words conn who -- =<< get'persons'words who
  Text msg _ -> print msg >> reply'json @Text conn "idk/text"
  Binary msg -> print msg >> reply'json @Text conn "idk/bin"

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

(.&) = M.intersection
(.-) = M.difference

score'submissions :: Gobble -> (Map Text Int, Gobble)
score'submissions gob = (all'subs, gob') where
  gob' = gob
    & players.traversed %~ scr
    & game'phase .~ Scoring
    & current'round +~ 1
  solution = gob^.board.word'list
  all'subs = gob ^.. players.traversed.answers & M.unionsWith (+)
  scr (Player conn sol scr) = Player conn sol (sum pts - sum npts) where
    pts = [ score'word word | (word,1) <- M.toList (all'subs .& sol .& solution) ]
    npts = [ score'word word | (word,1) <- M.toList (all'subs .& (sol .- solution)) ]

score'round :: (?gobble :: TVar Gobble, MonadIO m) => m ()
score'round = liftIO $ do
  (all'subs,gob) <- score'submissions <$> readTVarIO ?gobble
  atomically $ writeTVar ?gobble gob
  putStrLn "Scored Round... "
  broadcast'val $ tag'thing "words" $ renderHtml mempty
  let sol = gob ^. board.word'list
      wl = sortBy (flip compare `on` T.length . fst) $ M.toList sol
      subs = gob^..players.traversed.answers
  broadcast'val $ tag'thing "solution" $ renderHtml $ do
    h3 "word list"
    table $ forM_ wl $ \(w,d) -> tr $ td (text w) >> td (text d)
  broadcast'val $ tag'thing "scores" $ renderHtml $
    table $ do thead $ do td $ ""
                          mapM_ (th.text) (gob^.players.to M.keys)
               tr $ do td "score"
                       forM_ (gob^..players.traversed.score) $ \n ->
                         td ! H.style "text-align:center;" $ text $ T.pack $ show n
               tr $ do td "mistakes"
                       forM_ [ M.keys $ sub .- sol | sub <- subs ] $ \ws ->
                         td $ ul $ mapM_ (li.text) ws
               tr $ do td "words"
                       forM_ [ M.keys $ sub .& sol | sub <- subs ] $ \ws ->
                         td $ ul $ mapM_ (li.text) ws

-- horrible way of avoiding browser cache
html'of'board :: Board -> Html
html'of'board b = img ! H.src ("static/board.svg?" <> stringValue (b^.letters&hash&show))

fresh'round :: (?gobble :: TVar Gobble, MonadIO m) => m ()
fresh'round = liftIO $ do
  b <- new'board
  gob <- (board .~ b) . (game'phase .~ Boggled) .
         (players.traversed.answers .~ M.empty) <$>
         readTVarIO ?gobble
  atomically $ writeTVar ?gobble gob
  putStrLn "New Round"
  broadcast'val $ tag'thing "board" $ renderHtml $ html'of'board b
  broadcast'val $ A.object
    [ "time" A..= (b^.creationTime.to (addUTCTime round'period))
    , "pause" A..= score'length
    , "round" A..= round'length ]
  broadcast'val $ tag'thing "scores" $ renderHtml mempty
  broadcast'val $ tag'thing "solution" $ renderHtml mempty

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
  flip finally (remove'player who) $ forever $ receive conn >>= \case
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
      link ! H.rel "stylesheet" ! H.href "static/gobble.css?0"
      script ! H.src "static/jquery-3.4.1.slim.js" $ ""
      script ! H.src "static/gobble.js?0" $ ""
    H.body $ do
      H.h1 "GOBBLE"
      H.div ! H.class_ "row" $ do
        H.div ! H.class_ "column" $ do
          H.div ! H.id "gobble" $ ""
          
        H.div ! H.class_ "column" $ do
          H.div ! H.id "timer" $ ""
          H.form ! H.id "mush" $ do
            H.input ! H.autocomplete "off" ! H.spellcheck "off"
              ! H.type_ "text" ! H.id "scratch"
            H.input ! H.type_ "submit" ! H.value "mush!"
          H.ul ! H.id "submissions" $ ""
          
        H.div ! H.class_ "column" $ do
          H.div ! H.id "twitter" $ do
            H.div ! H.id "tweets" $ ""
          H.form ! H.id "tweet" $ do
            H.input ! H.type_ "text" ! H.autocomplete "off" ! H.id "scribble"
            H.input ! H.type_ "submit" ! H.hidden ""

      H.div ! H.class_ "row" $ do
        H.div ! H.class_ "column" $ do
          H.div ! H.id "solution" $ ""
        H.div ! H.id "scores" $ ""

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
  return $ unlines [gob ^.. players & show,gob ^.. chat'room & show]

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
         timer'thread <- forkIO $ forever run'round
         let bog = run port $ websocketsOr defaultConnectionOptions boggle back
         bog `finally` killThread timer'thread

main :: IO ()
main = map read <$> getArgs >>= \case
  [] -> launch'boggle 9009 -- ... ?
  x:_ -> launch'boggle x
