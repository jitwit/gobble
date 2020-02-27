{-# language OverloadedStrings, ImplicitParams, TemplateHaskell, LambdaCase #-}

module Gobble.Core where

import Data.Time.Clock
import Network.WebSockets
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Text as T
import Data.Text (Text)
import Control.Lens
import Unsafe.Coerce

todo = error "todo"

type Name = Text

data Phase = Boggled | Scoring
  deriving (Eq,Show)

data Board = Board
  { _creationTime :: UTCTime
  , _letters :: Text
  , _word'list :: Map Text Text
  } deriving (Show)

data Player = Player
  { _connection :: Connection
  , _answers :: Map Text Int
  , _score :: Int }

instance Show Player where
  show (Player _ as n) =
    "Player { _answers = " <> show as <> ", _score = " <> show n <> " }"

data Gobble = Gobble
  { _board :: Board
  , _players :: Map Name Player
  , _game'phase :: Phase
  , _current'round :: Int } deriving (Show)

makePrisms ''Phase
makeLenses ''Board
makeLenses ''Player
makeLenses ''Gobble

score'word :: Text -> Int
score'word = ([0,0,0,1,1,2,3,5,11] !!) . min 8 . T.length

round'length :: Int
round'length = 90

score'length :: Int
score'length = 30

round'period :: NominalDiffTime
round'period = unsafeCoerce $ secondsToDiffTime $
  fromIntegral $ round'length + score'length

newtype WhoElse = WhoElse { who'else :: [Name] }

newtype Answers = Answers { answer'list :: [Text] }

data ScoreReport = ScoreReport
  { report'participants :: [Name]
  , report'scores :: [Int]
  , report'mistakes :: [[Text]]
  , report'words :: [[Text]] }

-- reply :: (?gobble :: TVar Gobble, WebSocketsData a, MonadIO m) => Connection -> a -> m ()
-- reply conn = liftIO . sendTextData conn
-- 
-- reply'json :: (?gobble :: TVar Gobble, A.ToJSON j, MonadIO m) => Connection -> j -> m ()
-- reply'json conn = reply conn . A.encode

