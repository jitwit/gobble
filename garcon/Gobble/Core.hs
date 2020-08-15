{-# language OverloadedStrings, ImplicitParams, TemplateHaskell, LambdaCase #-}
{-# language TypeApplications, FlexibleContexts #-}

module Gobble.Core where

import Data.Default
import Data.Time.Clock
import Network.WebSockets
import qualified Data.Map as M
import Data.Map (Map)
import Data.HashMap.Strict (HashMap)
import qualified Data.Text as T
import Unsafe.Coerce
import Data.Text (Text)
import Control.Lens
import Control.Concurrent

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

data Activity = Here | There deriving (Show,Eq,Ord)

data Player = Player
  { _answers :: Map Text Int
  , _score :: Int
  , _solo'score :: Int
  , _total'score :: Int
  , _last'activity :: UTCTime
  , _active :: Activity
  } deriving (Show)

data Chat'Message = Chat'Message
  { _contents :: Text
  , _author :: Name } deriving Show

newtype Chat'View = Chat'View Gobble
newtype Score'View = Score'View Gobble
newtype Word'List'View = Word'List'View Gobble
data Player'Status = Player'Status Text Activity

data Chat = Chat { _messages :: Map UTCTime Chat'Message } deriving Show

data Gobble = Gobble
  { _current'round :: Int
  , _board :: Board
  , _players :: Map Name Player
  , _connections :: Map Name Connection
  , _game'phase :: Phase
  , _chat'room :: Chat
  , _pinou'stream :: [FilePath]
  , _gobble'likes :: Map Name Text
  , _english :: HashMap Text Text
  } -- deriving (Show)

type Game'Log = (Text,Int,Map Text ([Text],Int,Activity,UTCTime))

makePrisms ''Phase
makeLenses ''Board
makeLenses ''Player
makeLenses ''Gobbler
makeLenses ''Gobble
makeLenses ''Chat'Message
makeLenses ''Chat

instance Default Phase where
  def = Ready

score'word :: Text -> Int
score'word = ([0,0,0,1,1,2,3,5,11] !!) . min 8 . T.length

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

threadDelayS :: Integer -> IO ()
threadDelayS = threadDelay . fromIntegral . (*10^6) 

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
    spts = sum [ score'word word * (wgt (word `M.member` wl)) | word <- M.keys sol ]
    ppts = [ score'word word | (word,1) <- M.toList $ all'subs .* sol .* wl ]
    npts = [ score'word word | (word,1) <- M.toList $ all'subs .* (sol .- wl) ]

score'submissions :: Gobble -> Gobble
score'submissions gob = gob & players.~result & game'phase.~Scoring where
  result = calculate'scores (gob^.current'round.to signum
                            ,gob^.board.word'list
                            ,gob^.players)

game'log'view :: Gobble -> Game'Log
game'log'view gob = (gob^.board.letters,gob^.current'round,gob^.players<&>vp) where
  vp p = ((p^.answers & M.keys),p^.total'score,p^.active,p^.last'activity)

update'activity'1 :: UTCTime -> Player -> Player
update'activity'1 now who
  | ages'ago < dt = who & active .~ There
  | otherwise = who & active .~ Here
  where dt = diffUTCTime now (who ^. last'activity) & unsafeCoerce

update'activity :: UTCTime -> Gobble -> (Bool, Gobble)
update'activity now gob = (flag, gob') where
  flag = and $ zipWith (==)
                       (gob'^..players.folded.active)
                       (gob ^..players.folded.active)
  gob' = gob & players . mapped %~ update'activity'1 now
