{-# language FlexibleContexts #-}

module Gobble.Render where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import qualified Data.Text as T
import Data.Text (Text)
import Data.Word
import Data.Colour.SRGB

import Gobble.Outils

board'dia :: Text -> Diagram B
board'dia b = vcat [ hcat [ block x | x <- T.unpack r ] | r <- board'rows b ] where
  block x =
    let ls = case x of
          'Q' -> "Qu"
          _ -> [x]
        back = square 1
          & bg (sRGB24 0xF1 0xD0 0xF3)
        fore = text ls
          & scale 0.45
          & translate (V2 0 (-0.025))
          & font "Georgia"
     in fore <> back

write'board :: Text -> IO ()
write'board = renderSVG "static/board.svg" (mkSizeSpec (Just <$> V2 300 300)) . board'dia
