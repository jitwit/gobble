{-# language FlexibleContexts #-}

module Gobble.Render where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import qualified Data.Text as T
import Data.Text (Text)

import Gobble.Outils

board'dia :: Text -> Diagram B
board'dia b = vcat [ hcat [ square 1 <> snugCenter (pad 1 $ text [x])
                          | x <- T.unpack r ]
                   | r <- board'rows b ]

run'board :: Text -> IO ()
run'board = renderSVG "board.svg" (mkSizeSpec (Just <$> V2 500 500)) . board'dia
  
