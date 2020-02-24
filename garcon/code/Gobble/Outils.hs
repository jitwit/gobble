{-# language OverloadedStrings #-}

module Gobble.Outils where

import qualified Data.Text as T
import Data.Text (Text)

todo = error "todo"

isqrt :: Integral a => a -> a
isqrt = floor . sqrt . fromIntegral

board'rows :: Text -> [Text]
board'rows b = T.chunksOf (isqrt $ T.length b) b
