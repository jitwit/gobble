{-# language OverloadedStrings #-}

module Main where

import Gobble.Core
import Control.Lens
import qualified Data.Map as M
import Test.Hspec

fanny, alexander :: Player
fanny = Player (M.fromList [("cat",1),("blaha",1),("maples",1)]) 0 0 10
alexander = Player (M.fromList [("mouse",1),("haha",1),("blaha",1)]) 0 0 10

player'list = M.fromList [("fanny",fanny),("alexander",alexander)]
solution = M.fromList [("cat","to vomit")
                      ,("mouse","small rodent/to hunt mice")
                      ,("maples","maple, a hardwood tree")]
player'scores'0 = calculate'scores (0,solution,player'list)
player'scores'1 = calculate'scores (1,solution,player'list)

infixr 0 ==>
(==>) :: (HasCallStack, Show a, Eq a) => a -> a -> Expectation
(==>) = shouldBe

main = do
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

