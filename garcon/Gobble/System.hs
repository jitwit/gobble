{-# language OverloadedStrings, LambdaCase, ViewPatterns, BangPatterns #-}

module Gobble.System where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Int
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import Text.Printf
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import System.Directory
import System.Random
import System.Random.Shuffle
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Default
import Database.HDBC
import Database.HDBC.Sqlite3

import Gobble.Dawggle
import qualified Gobble.Dawg as D
import Gobble.Render
import Gobble.Core

new'board :: D.Node -> H.HashMap T.Text Boggle'Word -> V.Vector String -> IO Board
new'board d h w = do
  b <- T.pack . snd <$> roll w
  let bs = boggle'search d $ T.unpack b
  t <- getCurrentTime
  write'board b
  return $ Board t b $ M.fromList [ (w,h H.! w ^. definition) | w <- T.pack <$> bs ]

new'board'gobble :: Gobble -> IO Board
new'board'gobble g = new'board (g^.gobble'dawg) (g^.english) (g^.gobble'big'words)

start'state :: IO Gobble
start'state = do
  (d,ws) <- fetch'dict
  col <- retrieve'dictionary
  b0 <- new'board d col ws
  pinous <- make'pinou'stream
  conn <- connect'db
  setup'db conn
  let g0 = Gobble (-1)
                  b0
                  mempty
                  mempty
                  def
                  (Chat mempty)
                  pinous
                  mempty
                  col
                  d
                  ws
                  conn
  set'previously'seen g0

retrieve'dictionary :: IO (H.HashMap T.Text Boggle'Word)
retrieve'dictionary =
  let parse'line (T.breakOn "\t" -> (w,d)) = (w, Boggle'Word False $ T.drop 1 d)
   in H.fromList . map parse'line . T.lines <$> T.readFile "static/definitions.txt"

include'previously'seen :: [Text] -> Gobble -> Gobble
include'previously'seen ws g = g & english %~ up where
  up d = L.foldl' (flip $ H.adjust $ been'seen .~ True) d ws

set'previously'seen :: MonadIO m => Gobble -> m Gobble
set'previously'seen g = do
  ws <- query'db g query'distinct'words
  return $! include'previously'seen ws g

update'previously'seen :: Gobble -> Gobble
update'previously'seen g = include'previously'seen ws g where
  ws = M.keys $ M.unionsWith (+) $ g ^.. players.folded.answers

make'pinou'stream :: IO [FilePath]
make'pinou'stream =
  let img'dir = "static/images/"
   in do imgs <- fmap (img'dir<>) . V.fromList <$> listDirectory img'dir
         gen <- newStdGen
         return [ imgs V.! j | j <- randomRs (0,V.length imgs - 1) gen ]

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
  ,   "name TEXT NOT NULL,"
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
  => Connection -> SqlValue -> (Name,Player) -> m SqlValue
record'solution c bid (n,p) = liftIO $ do
  s <- prepare c "INSERT INTO solution VALUES (?,?,?,?)"
  execute s [n&toSql,p^.score&toSql,p^.solo'score&toSql,bid]
  commit c
  head.head <$> quickQuery' c "SELECT last_insert_rowid()" []

record'words :: (MonadIO m)
  => Connection -> [SqlValue] -> [[Text]] -> m ()
record'words c ps wss = liftIO $ do
  s <- prepare c "INSERT INTO word VALUES (?,?)"
  forM_ (zip ps wss) $ \(p, ws) ->
    forM_ ws $ \w -> execute s [w&toSql,p]
  commit c

record'round :: (MonadIO m) => Gobble -> m ()
record'round g = liftIO $ do
  let c = g^.gobble'connection
  bid <- record'board c (g^.current'round) (g^.board)
  ids <- forM (g^@..players.itraversed) $ record'solution c bid
  record'words c ids (g^..players.folded.answers.to M.keys)

-- QUERY
query'past'users :: (MonadIO m) => Connection -> m [Name]
query'past'users c = liftIO $
  map fromSql . join <$> quickQuery' c "select distinct name from solution;" []

query'last'boardid :: (MonadIO m) => Connection -> m (Maybe Int64)
query'last'boardid c = liftIO $ do
 res <- quickQuery' c "select rowid from board order by rowid desc limit 1;" []
 pure $ case join res of
   x:_ -> Just $ fromSql x
   _ -> Nothing

query'last'round :: (MonadIO m) => Connection -> m [[SqlValue]]
query'last'round c = liftIO $ query'last'boardid c >>= \case
  Nothing -> pure []
  Just j -> do
    let s = printf "select * from solution where solution.boardid == %d" j
    quickQuery' c s []

query'all'my'words :: (MonadIO m) => Name -> Connection -> m [Text]
query'all'my'words who c = liftIO $ do
  let s = unlines
            [ "select * from word inner join solution"
            , "where word.solutionid == solution.rowid and"
            , printf "solution.name == '%s'" who
            , "order by length(word.letters) desc" ]
  map (fromSql.head) <$> quickQuery' c s []

query'distinct'words :: (MonadIO m) => Connection -> m [Text]
query'distinct'words c = liftIO $ do
  map fromSql . join <$> quickQuery' c "select distinct letters from word" []

query'all'solutions :: (MonadIO m) => Connection -> m [(Text,Text)]
query'all'solutions c = liftIO $ do
  let s = unlines
            [ "select * from word inner join solution"
            , "where word.solutionid == solution.rowid" ]
  res <- quickQuery' c s []
  return [ (fromSql w,fromSql p) | w:_:p:_ <- res ]

query'solvers :: (MonadIO m) => Text -> Connection -> m [Text]
query'solvers word c = liftIO $ do
  let s = unlines
            [ "select letters,name from word inner join solution"
            , "where word.solutionid == solution.rowid and"
            , printf "word.letters == '%s'" word
            , "group by name" ]
  res <- quickQuery' c s []
  return $ [ fromSql w | [_,w] <- res ]

fetch'all'solutions :: (MonadIO m) => Gobble -> m (M.Map Text [Text])
fetch'all'solutions g = do
  let d = g^.english
  res <- query'db g query'all'solutions
  return $ M.fromListWith (<>) [ (w,[p]) | (w,p) <- res, w `H.member` d ]

best'words :: (MonadIO m) => Gobble -> Name -> Int -> m [Text]
best'words g who n = do
  let d = g^.english
  ws <- query'db g (query'all'my'words who)
  return $ take n [ w | w <- ws, w `H.member` d ]

past'solvers :: (MonadIO m) => Gobble -> Text -> m [Text]
past'solvers g w = query'db g (query'solvers w)

run'update'solution'name :: (MonadIO m) => Name -> Name -> Connection  -> m ()
run'update'solution'name a b c = liftIO $ do
  let s = unlines
            [ "update solution"
            , printf "set name = '%s'" b
            , printf "where name = '%s';" a ]
  quickQuery' c s []
  commit c

query'db :: (MonadIO m) => Gobble -> (Connection -> m a) -> m a
query'db g q = q (g^.gobble'connection)

-- COM
connect'db :: MonadIO m => m Connection
connect'db = liftIO $ connectSqlite3 db'file

dummy :: IO ()
dummy = do
  c <- connect'db
  print =<< query'all'my'words ("pinou"::Text) c
  disconnect c
