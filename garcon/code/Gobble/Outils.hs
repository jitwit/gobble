{-# language OverloadedStrings #-}

module Gobble.Outils where

import Data.Time.Clock
import Control.Concurrent
import qualified Data.Text as T
import Data.Text (Text)
import Unsafe.Coerce

todo = error "todo"

isqrt :: Integral a => a -> a
isqrt = floor . sqrt . fromIntegral

board'rows :: Text -> [Text]
board'rows b = T.chunksOf (isqrt $ T.length b) b

threadDelayS :: Int -> IO ()
threadDelayS = threadDelay . (*10^6)

round'length, score'length :: Int
round'length = 90
score'length = 30

round'period :: NominalDiffTime
round'period = unsafeCoerce $ secondsToDiffTime $
  fromIntegral $ round'length + score'length

