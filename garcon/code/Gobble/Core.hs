{-# language OverloadedStrings, ImplicitParams, TemplateHaskell, LambdaCase #-}

module Gobble.Core where

import Data.Time.Clock
import Network.WebSockets
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)
import Control.Lens
import Unsafe.Coerce
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

makePrisms ''Phase
makeLenses ''Board
makeLenses ''Player
makeLenses ''Gobble
makeLenses ''Chat'Message
makeLenses ''Chat

score'word :: Text -> Int
score'word = ([0,0,0,1,1,2,3,5,11] !!) . min 8 . T.length

round'length :: Int
round'length = 90

score'length :: Int
score'length = 45

round'period :: NominalDiffTime
round'period = unsafeCoerce $ secondsToDiffTime $
  fromIntegral $ round'length + score'length

isqrt :: Integral a => a -> a
isqrt = floor . sqrt . fromIntegral

board'rows :: Text -> [Text]
board'rows b = T.chunksOf (isqrt $ T.length b) b

threadDelayS :: Int -> IO ()
threadDelayS = threadDelay . (*10^6)

dup :: a -> (a,a)
dup x = (x,x)
