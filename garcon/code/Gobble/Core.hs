{-# language OverloadedStrings, ImplicitParams, TemplateHaskell, LambdaCase, FlexibleContexts #-}

module Gobble.Core where

import Data.Time.Clock
import Network.WebSockets
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Text as T
import Unsafe.Coerce
import Data.Text (Text)
import Control.Lens
import Control.Concurrent

todo :: todo
todo = error "todo"

type Name = Text
type Reason = String

data Phase = Boggled | Scoring | Ready | Broken Reason
  deriving (Eq,Show)

data Board = Board
  { _creation'time :: UTCTime
  , _letters :: Text
  , _word'list :: Map Text Text
  } deriving (Show)

data Player = Player
  { _connection :: Connection
  , _answers :: Map Text Int
  , _score :: Int
  , _solo'score :: Int
  , _total'score :: Int }

instance Show Player where
  show (Player _ as n s t) =
    "Player { _answers = " <> show as <> ", _score = " <> show n <> ", _total'score = " <> show t <> " }"

data Chat'Message = Chat'Message
  { _contents :: Text
  , _author :: Name } deriving Show

newtype Chat'View = Chat'View Gobble
newtype Score'View = Score'View Gobble
newtype Word'List'View = Word'List'View Gobble

data Chat = Chat { _messages :: Map UTCTime Chat'Message } deriving Show

data Gobble = Gobble
  { _board :: Board
  , _players :: Map Name Player
  , _game'phase :: Phase
  , _chat'room :: Chat
  , _current'round :: Int } deriving (Show)

type Game'Log = (Text,Int,Map Text ([Text],Int))

makePrisms ''Phase
makeLenses ''Board
makeLenses ''Player
makeLenses ''Gobble
makeLenses ''Chat'Message
makeLenses ''Chat

score'word :: Text -> Int
score'word = ([0,0,0,1,1,2,3,5,11] !!) . min 8 . T.length

round'length :: Integer
round'length = 20

score'length :: Integer
score'length = 10

overall'length :: Integer
overall'length = round'length + score'length

step'length :: Int
step'length = 500000

round'period :: NominalDiffTime
round'period = unsafeCoerce $ secondsToDiffTime $
  fromIntegral $ round'length + score'length

isqrt :: Integral a => a -> a
isqrt = floor . sqrt . fromIntegral

board'rows :: Text -> [Text]
board'rows b = T.chunksOf (isqrt $ T.length b) b

threadDelayS :: Integer -> IO ()
threadDelayS = threadDelay . fromIntegral . (*10^6) 

dup :: a -> (a,a)
dup x = (x,x)

(.&) = M.intersection
(.-) = M.difference

score'submissions :: Gobble -> Gobble
score'submissions gob = gob & players.traversed%~scr'rnd & game'phase.~Scoring where
  new = gob ^. current'round.to signum
  wgt b = if b then 1 else -1
  solution = gob^.board.word'list
  all'subs = gob ^.. players.traversed.answers & M.unionsWith (+)
  scr'rnd p@(Player conn sol scr ssr tot) = p' where
    p' = p & score .~ pts & solo'score .~ spts & total'score .~ pts+tot*new
    tot = p ^. total'score
    pts = sum ppts - sum npts
    spts = sum [ score'word word * (wgt (word `M.member` solution)) | word <- M.keys sol ]
    ppts = [ score'word word | (word,1) <- M.toList (all'subs .& sol .& solution) ]
    npts = [ score'word word | (word,1) <- M.toList (all'subs .& (sol .- solution)) ]

game'log'view :: Gobble -> Game'Log
game'log'view gob = (gob^.board.letters,gob^.current'round,gob^.players<&>vp) where
  vp p = ((p^.answers & M.keys),p^.total'score)
