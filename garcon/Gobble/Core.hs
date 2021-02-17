{-# language OverloadedStrings, ImplicitParams, TemplateHaskell, LambdaCase #-}
{-# language TypeApplications, FlexibleContexts, DisambiguateRecordFields #-}
{-# language DuplicateRecordFields #-}

module Gobble.Core where

import qualified Data.ByteString.Lazy.UTF8 as B8
import Data.Default
import Data.Time.Clock
import Network.WebSockets
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Vector as V
import Data.HashMap.Strict (HashMap)
import qualified Data.Text as T
import Unsafe.Coerce
import Data.Text (Text)
import qualified Database.HDBC.Sqlite3 as DB
import Control.Lens
import Control.Concurrent

import qualified Gobble.Dawg as D

todo :: todo
todo = error "todo"

type Name = Text

data Phase = Boggled | Scoring | Ready
  deriving (Eq,Show)

data Board = Board
  { _creation'time :: UTCTime
  , _letters :: Text
  , _word'list :: Map Text Text
  } deriving (Show)

data Status = Here | There deriving (Show,Eq,Ord)

data Player = Player
  { _answers :: Map Text Int
  , _score :: Int
  , _solo'score :: Int
  , _total'score :: Int
  , _last'activity :: UTCTime
  , _status :: Status
  } deriving (Show)

data Name'Check = Name'OK | Name'Taken | Name'Too'Long | Name'Is'Empty
  deriving (Show,Eq)

data Chat'Message = Chat'Message
  { _contents :: Text
  , _author :: Name } deriving Show

data Boggle'Word = Boggle'Word
  { _been'seen :: Bool, _definition :: Text }
  deriving (Show)
type Dictionary = HashMap Text Boggle'Word
newtype Chat'View = Chat'View Room
data Score'View = Score'View Dictionary Room
newtype Word'List'View = Word'List'View Room
newtype Score'Preview = Score'Preview Room
data Player'Status = Player'Status Text Status
newtype Round'View = Round'View Int

data Chat = Chat { _messages :: Map UTCTime Chat'Message } deriving Show

data Gobble = Gobble
  { _arena :: Room -- soon to be map name room
  , _connections :: Map Name Connection
  , _pinou'stream :: [FilePath]
  , _english :: HashMap Text Boggle'Word
  , _gobble'dawg :: D.Node
  , _gobble'big'words :: V.Vector String
  , _gobble'connection :: DB.Connection }

data Visibility = Global | Private [Text]

data Room = Room
  { _name :: Name
  , _players :: Map Name Player
  , _chat :: Chat
  , _likes :: Map Name Text
  , _phase :: Phase
  , _current'round :: Int
  , _board :: Board
  , _visibility :: Visibility }

instance Default Board where
  def = Board (unsafeCoerce 0) "" mempty

instance Default Chat where
  def = Chat mempty

instance Default Room where
  def = Room "home" mempty def mempty Ready (-1) def Global

data Status'Query = Who'Query | Word'List'Query
  deriving (Show)
-- besides Chirp, messages (requests mostly) that the client sends to
-- update state
data Gobble'Message
  = Status'Message Status'Query
  | Submit'Message [T.Text]
  | Delete'Message T.Text
  | Like'Message T.Text
  | Chirp'Message Chirp
  | IDK'Message T.Text
  deriving (Show)

data Chirp
  = Help'Me
  | Define T.Text
  | Who's'Gotten T.Text
  | Chirp T.Text
  deriving (Show)

-- should use real parser, no?
parse'ws'message :: DataMessage -> Gobble'Message
parse'ws'message (Text t _) =
  let t' = T.pack $ B8.toString t
  in case T.words $ T.toUpper t' of
    "GOBBLE":ws -> Submit'Message ws
    "DOBBLE":w:[] -> Delete'Message w
    "WOBBLE":w:[] -> Like'Message w
    _ -> case T.stripPrefix "chirp " t' of
           Just msg -> case T.words msg of
             ["?def",w] -> Chirp'Message $ Define w
             ["?who",w] -> Chirp'Message $ Who's'Gotten w
             ["?help"] -> Chirp'Message $ Help'Me
             _ -> Chirp'Message $ Chirp msg
           Nothing -> case t' of
             "who-else" -> Status'Message Who'Query
             "words" -> Status'Message Word'List'Query
--             "preview" -> Status'Message Score'Preview'Query
             _ -> IDK'Message t'
parse'ws'message (Binary m) = IDK'Message $ T.pack $ B8.toString m

type Game'Log = (Text,Int,Map Text ([Text],Int,Status,UTCTime))

makePrisms ''Phase
makeLenses ''Board
makeLenses ''Room
makeLenses ''Player
makeLenses ''Gobble
makeLenses ''Chat'Message
makeLenses ''Boggle'Word
makeLenses ''Chat

instance Default Phase where
  def = Ready

score'word :: Text -> Int
score'word = ((0:0:fibs)!!) . min 9 . T.length where
  fibs = 0:zipWith (+) fibs (1:fibs)

round'length :: Integer
round'length = 90
score'length :: Integer
score'length = 45

overall'length :: Integer
overall'length = round'length + score'length

run'length, ready'length, ages'ago :: Int
run'length = 50000
ready'length = 500000
ages'ago = 1000*1000*1000*1000*90

round'period :: NominalDiffTime
round'period = unsafeCoerce $ secondsToDiffTime $
  fromIntegral $ round'length + score'length

isqrt :: Integral a => a -> a
isqrt = floor . sqrt . fromIntegral

board'rows :: Text -> [Text]
board'rows b = T.chunksOf (isqrt $ T.length b) b

dup :: a -> (a,a)
dup x = (x,x)

-- | just because...
infixl 7 .-
infixl 6 .+
infixl 6 .*
(.*) = M.intersection @Text
(.-) = M.difference @Text
(.+) = M.union @Text

calculate'scores :: (Int,Map Text Text,Map Name Player) -> Map Name Player
calculate'scores (new,wl,ps) = ps & mapped %~ scr where
  wgt b = 2 * fromEnum b - 1
  all'subs = ps^..folded.answers & M.unionsWith (+)
  scr p@(Player sol scr ssr tot _ _) = p' where
    p' = p & score .~ pts & solo'score.~spts & total'score.~pts+tot*new
    tot = p^.total'score
    pts = sum ppts - sum npts
    spts = sum [ score'word word * wgt (word `M.member` wl) | word <- M.keys sol ]
    ppts = [ score'word word | (word,1) <- M.toList $ all'subs .* sol .* wl ]
    npts = [ score'word word | (word,1) <- M.toList $ all'subs .* (sol .- wl) ]

score'submissions :: Room -> Room
score'submissions gob = gob & players.~result & phase.~Scoring where
  result = current'scores gob

current'scores :: Room -> Map Name Player
current'scores gob = calculate'scores
  (gob^.current'round.to signum,gob^.board.word'list,gob^.players)

--game'log'view :: Gobble -> Game'Log
--game'log'view gob = (gob^.board.letters,gob^.current'round,gob^.players<&>vp) where
--  vp p = ((p^.answers & M.keys),p^.total'score,p^.status,p^.last'activity)

update'activity'1 :: UTCTime -> Player -> Player
update'activity'1 now who
  | ages'ago < dt = who & status .~ There
  | otherwise = who & status .~ Here
  where dt = diffUTCTime now (who ^. last'activity) & unsafeCoerce
        pl = who ^. status

update'activity :: UTCTime -> Gobble -> (Bool,Gobble)
update'activity now gob = (flag, gob') where
  flag = and $ zipWith (==)
                       (gob'^..arena.players.folded.status)
                       (gob ^..arena.players.folded.status)
  gob' = gob & arena . players . mapped %~ update'activity'1 now
