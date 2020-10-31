-- logginig/recording

module Gobble.Memory where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Int
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import Database.HDBC as DB
import Database.HDBC.Sqlite3
import System.Directory

import Gobble.Core

-- CONFIG
db'file = "data/gobble.db"

-- SETUP
setup'db :: Connection -> IO ()
setup'db c = do
  run c create'board'table []
  run c create'solution'table []
  run c create'word'table []
  commit c

create'board'table = unlines
  [ "CREATE TABLE IF NOT EXISTS board ("
  ,   "letters TEXT NOT NULL,"
  ,   "time INTEGER NOT NULL,"
  ,   "round INTEGER NOT NULL"
  , ");" ]

create'solution'table = unlines
  [ "CREATE TABLE IF NOT EXISTS solution ("
  ,   "who TEXT NOT NULL,"
  ,   "score INTEGER NOT NULL,"
  ,   "soloscore INTEGER NOT NULL,"
  ,   "boardid INTEGER NOT NULL,"
  ,   "FOREIGN KEY (boardid)"
  ,     "REFERENCES board (boardid)"
  , ");" ]

create'word'table = unlines
  [ "CREATE TABLE IF NOT EXISTS word ("
  ,   "letters TEXT NOT NULL,"
  ,   "solutionid INTEGER NOT NULL,"
  ,   "FOREIGN KEY (solutionid)"
  ,     "REFERENCES solution (solutionid)"
  , ");" ]

-- INSERTION
since'epoch :: UTCTime -> Int64
since'epoch = floor . (*10^9) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

record'board :: (MonadIO m) => Connection -> Int -> Board -> m SqlValue
record'board c r b = liftIO $ do
  s <- prepare c "INSERT INTO board VALUES (?,?,?)"
  execute s [b^.letters&toSql,b^.creation'time&since'epoch&toSql,r&toSql]
  commit c
  -- too "unsafe"?
  head.head <$> quickQuery' c "SELECT last_insert_rowid()" []

record'solution :: (MonadIO m)
  => Connection -> SqlValue -> Name -> Player -> m SqlValue
record'solution c bid n p = liftIO $ do
  s <- prepare c "INSERT INTO solution VALUES (?,?,?,?)"
  execute s [n&toSql,p^.score&toSql,p^.solo'score&toSql,bid]
  commit c
  head.head <$> quickQuery' c "SELECT last_insert_rowid()" []

record'words :: (MonadIO m)
  => Connection -> [SqlValue] -> [[Text]] -> m ()
record'words c ps wss = liftIO $ do
  s <- prepare c "INSERT INTO word VALUES (?,?)"
  forM_ (zip ps wss) $ \(p, ws) ->
    forM_ ws $ \w -> execute s [p,w&toSql]
  commit c

record'round :: (MonadIO m) => Gobble -> m ()
record'round g = liftIO $ do
  let c = g^.gobble'db'conn
  bid <- record'board c (g^.current'round) (g^.board)
  ids <- forM (g^@..players.itraversed) $ \(n,p) -> record'solution c bid n p
  record'words c ids (g^..players.folded.answers.to M.keys)

-- COM
connect'db :: MonadIO m => m Connection
connect'db = liftIO $ connectSqlite3 db'file

-- dummy :: IO ()
-- dummy = do
--   b <- doesFileExist db'file
--   when b $ removeFile db'file
--   print "load"
--   g <- start'state
--   print "gen"
--   b <- new'board'gobble g
--   print "con"
--   c <- connect'db
--   setup'db c
--   bid <- record'board c 0 b
--   print bid
-- --  print <- record'player c bid "pinou" 10
--   disconnect c
