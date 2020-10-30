-- logginig/recording

module Gobble.Memory where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Text (Text)
import Data.Int
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Directory

import Gobble.Core
import Gobble.System

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
  ,   "time INTEGER NOT NULL"
  , ");" ]

create'solution'table = unlines
  [ "CREATE TABLE IF NOT EXISTS solution ("
  ,   "who TEXT NOT NULL,"
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
since'epoch = floor . (1e9 *) . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

record'board :: (MonadIO m) => Connection -> Board -> m SqlValue
record'board c b = liftIO $ do
  s <- prepare c "INSERT INTO board VALUES (?,?)"
  execute s [b^.letters&toSql, b^.creation'time&since'epoch&toSql]
  commit c
  head.head <$> quickQuery' c "SELECT last_insert_rowid()" []

record'player :: (MonadIO m) => Connection -> SqlValue -> Text -> m SqlValue
record'player c bid p = liftIO $ do
  s <- prepare c "INSERT INTO solution VALUES (?,?)"
  execute s [p&toSql, bid]
  commit c
  head.head <$> quickQuery' c "SELECT last_insert_rowid()" []

record'words :: (MonadIO m) => Connection -> SqlValue -> SqlValue -> [Text] -> m ()
record'words c b p ws = liftIO $ do
  pure ()

-- COM
connect'db :: MonadIO m => m Connection
connect'db = liftIO $ connectSqlite3 db'file

dummy :: IO ()
dummy = do
  b <- doesFileExist db'file
  when b $ removeFile db'file
  print "load"
  g <- start'state
  print "gen"
  b <- new'board'gobble g
  print "con"
  c <- connect'db
  setup'db c
  bid <- record'board c b
  print bid
  print <- record'player c bid "pinou"
  disconnect c
