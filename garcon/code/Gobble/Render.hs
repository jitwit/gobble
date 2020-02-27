{-# language FlexibleContexts #-}

module Gobble.Render where

import Diagrams.Prelude as D
import Diagrams.Backend.SVG
import qualified Data.Text.Lazy as T
import Data.Text (Text)
import Data.Word
import Data.Colour.SRGB
import Text.Blaze.Html5 as H hiding (map,main,head,style)
import qualified Text.Blaze.Html5 as H (head)
import Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5.Attributes as H hiding (form)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import Network.WebSockets

import Gobble.Core
import Gobble.Outils

board'dia :: Text -> Diagram B
board'dia b = vcat [ hcat [ block x | x <- T.unpack r ] | r <- board'rows b ] where
  block x =
    let ls = case x of
          'Q' -> "Qu"
          _ -> [x]
        back = square 1
          & bg (sRGB24 0xF1 0xD0 0xF3)
        fore = D.text ls
          & scale 0.45
          & translate (V2 0 (-0.025))
          & font "Georgia"
     in fore <> back

write'board :: Text -> IO ()
write'board = renderSVG "static/board.svg" (mkSizeSpec $ Just <$> V2 300 300) . board'dia

tag'thing :: A.ToJSON v => Text -> v -> A.Value
tag'thing tag val = A.object [ tag A..= val ]

instance WebSocketsData WhoElse where
  toLazyByteString (WhoElse peeps) = B.pack $ T.unpack $ renderHtml $ do
    h3 "who's here?"
    ul ! H.id "definitions" $ mapM_ (li.H.text) peeps
