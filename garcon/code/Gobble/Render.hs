{-# language FlexibleContexts #-}

module Gobble.Render where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import qualified Data.Text as T
import Data.Text (Text)

import Gobble.Outils

board'dia :: Text -> Diagram B
board'dia b = vcat [ hcat [ block x | x <- T.unpack r ] | r <- board'rows b ] where
  block x = text [x] <> square 1
