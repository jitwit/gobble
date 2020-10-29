{-# language OverloadedStrings #-}

module Main where

import Gobble.Core
import Gobble.Dawggle
import Control.Lens
import Control.Monad
import qualified Data.Map as M
import qualified Data.Text as T
import Test.Hspec
import Data.Time

mk'player ws nw = Player ws 0 0 10 nw Here

solution = M.fromList [("cat","to vomit")
                      ,("mouse","small rodent/to hunt mice")
                      ,("maples","maple, a hardwood tree")]

infixr 0 ==>
(==>) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
(==>) = shouldBe

main = do
  now <- getCurrentTime
  (trie,big'words) <- fetch'dict
  sols <- replicateM 100 (roll big'words)
  let fanny = mk'player (M.fromList [("cat",1),("blaha",1),("maples",1)]) now
      alexander = mk'player (M.fromList [("mouse",1),("haha",1),("blaha",1)]) now
      player'list = M.fromList [("fanny",fanny),("alexander",alexander)]
      player'scores'0 = calculate'scores (0,solution,player'list)
      player'scores'1 = calculate'scores (1,solution,player'list)
  hspec $ describe "Score" $ do
    describe "new round" $ do
      it "round score fanny" $ do
        player'scores'0^?!ix "fanny".score ==> 4
      it "round score alexander" $ do
        player'scores'0^?!ix "alexander".score ==> 1
      it "solo score fanny" $ do
        player'scores'0^?!ix "fanny".solo'score ==> 2
      it "solo score alexander" $ do
        player'scores'0^?!ix "alexander".solo'score ==> -1
      it "total score fanny" $ do
        player'scores'0^?!ix "fanny".total'score ==> 4
      it "total score alexander" $ do
        player'scores'0^?!ix "alexander".total'score ==> 1

    describe "continuing round" $ do
      it "round score fanny" $ do
        player'scores'1^?!ix "fanny".score ==> 4
      it "round score alexander" $ do
        player'scores'1^?!ix "alexander".score ==> 1
      it "solo score fanny" $ do
        player'scores'1^?!ix "fanny".solo'score ==> 2
      it "solo score alexander" $ do
        player'scores'1^?!ix "alexander".solo'score ==> -1
      it "total score fanny" $ do
        player'scores'1^?!ix "fanny".total'score ==> 14
      it "total score alexander" $ do
        player'scores'1^?!ix "alexander".total'score ==> 11

    describe "removing qu" $ do
      it "removes qu" $ do
        (del'qu "QUASTHOFF") ==> "QASTHOFF"
      it "keeps no qu" $ do
        (del'qu "QATAR") ==> "QATAR"
      it "empty" $ do
        (del'qu "") ==> ""
      it "signle" $ do
        (del'qu "Q") ==> "Q"

    describe "removing qu" $ do
      it "removes qu" $ do
        (del'qu "QUASTHOFF") ==> "QASTHOFF"
      it "keeps no qu" $ do
        (del'qu "QATAR") ==> "QATAR"
      it "empty" $ do
        (del'qu "") ==> ""
      it "signle" $ do
        (del'qu "Q") ==> "Q"

    describe "word found in derived generated board" $
      forM_ sols $ \(wd,bd) -> do
        it (unwords [wd,"in",bd]) $ do
          (wd `elem` boggle'search trie bd)==> True

