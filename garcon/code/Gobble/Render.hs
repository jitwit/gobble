{-# language OverloadedStrings, ImplicitParams, TemplateHaskell, LambdaCase #-}
{-# language FlexibleContexts #-}

module Gobble.Render where

import Control.Monad
import Control.Lens
import qualified Diagrams.Prelude as D
import Diagrams.Prelude (Diagram,V2 (..))
import Diagrams.Backend.SVG
import qualified Data.Text as T
import qualified Data.Text.Internal.Lazy as TLI
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
import qualified Data.Map as M
import Data.List
import Data.Function

import Gobble.Core

type H = TLI.Text

board'dia :: Text -> Diagram B
board'dia b = D.vcat [ D.hcat [ block x | x <- T.unpack r ] | r <- board'rows b ] where
  block x =
    let ls = case x of
          'Q' -> "Qu"
          _ -> [x]
        back = D.square 1 & D.bg (sRGB24 0xF1 0xD0 0xF3)
        fore = D.text ls & D.scale 0.45 & D.translate (V2 0 (-0.025)) & D.font "Georgia"
     in fore <> back

write'board :: Text -> IO ()
write'board = renderSVG "static/board.svg" (D.mkSizeSpec $ Just <$> V2 300 300) . board'dia

tag'thing :: A.ToJSON v => Text -> v -> A.Value
tag'thing tag val = A.object [ tag A..= val ]

render'chat :: Gobble -> H
render'chat gob = renderHtml chat'html where
  whos'here = H.text $ T.intercalate ", " (gob^.players.to M.keys)
  chat'html = table $ do
    thead whos'here
    forM_ (gob^..chat'room.messages.folded) $ \(Chat'Message tweet author) ->
      tr $ do td $ H.text author
              td $ H.text tweet

(.&) = M.intersection
(.-) = M.difference

render'scores :: Gobble -> [(Text,H)]
render'scores gob = [("solution",solution),("scores",report)] where
  sol = gob ^. board.word'list
  wl = sortBy (flip compare `on` T.length . fst) $ M.toList sol
  subs = gob^..players.traversed.answers
  solution = renderHtml $ do
    h3 "word list"
    table $ forM_ wl $ \(w,d) -> tr $ td (H.text w) >> td (H.text d)
  report = renderHtml $ table $ do
    thead $ do td $ ""
               mapM_ (th.H.text) (gob^.players.to M.keys)
    tr $ do
      td "score"
      forM_ (gob^..players.traversed.score) $ \n ->
        td ! H.style "text-align:center;" $ H.text $ T.pack $ show n
    when (1 < length subs) $
      tr $ do td "solo"
              forM_ [ sum (map score'word (M.keys (sub .& sol))) -
                      sum (map score'word (M.keys (sub .- sol)))
                    | sub <- subs ] $ \ n ->
                td ! H.style "text-align:center;" $ H.text $ T.pack $ show n
    tr $ do td "mistakes"
            forM_ [ M.keys $ sub .- sol | sub <- subs ] $
              \ws -> td $ ul $ mapM_ (li.H.text) ws
    tr $ do td "words"
            forM_ [ M.keys $ sub .& sol | sub <- subs ] $ \ws ->
              td $ ul $ mapM_ (li.H.text) ws

html'of'board :: Board -> H
html'of'board b = renderHtml $ img ! H.src board'source where
  board'source = "static/board.svg?" <> stringValue (b^.letters&show)

html'of'pinou :: String -> H
html'of'pinou src = renderHtml $ H.img ! H.src (stringValue src)

render'words :: Name -> Gobble -> H
render'words who gob = renderHtml $
  mapM_ (li.H.text) $ gob^.players.ix who.answers.to M.keys

clear'html :: H
clear'html = renderHtml mempty

---- HOME PAGE
data GobblePage = GobblePage

instance ToMarkup GobblePage where
  toMarkup _ = html $ do
    H.head $ do
      title "gobble"
      link ! H.rel "stylesheet" ! H.href "static/gobble.css?4"
      script ! H.src "static/jquery-3.4.1.slim.js" $ ""
      script ! H.src "static/gobble.js?4" $ ""
    H.body $ do
      H.h1 "GOBBLE"
      H.div ! H.class_ "row" $ do
        H.div ! H.class_ "column" $ do
          H.div ! H.id "gobble" $ ""
        H.div ! H.class_ "column" $ do
          H.div ! H.id "timer" $ ""
          H.form ! H.id "mush" $ do
            H.input ! H.autocomplete "off" ! H.spellcheck "off"
              ! H.type_ "text" ! H.id "scratch"
            H.input ! H.type_ "submit" ! H.hidden "mush!"
          H.ul ! H.id "submissions" $ ""
          H.div ! H.id "pinou" $ ""
        H.div ! H.class_ "column" $ do
          H.div ! H.id "twitter" $ do
            H.div ! H.id "tweets" $ ""
          H.form ! H.id "tweet" $ do
            H.input ! H.type_ "text" ! H.autocomplete "off" ! H.id "scribble"
            H.input ! H.type_ "submit" ! H.hidden ""
      H.div ! H.class_ "row" $ do
        H.div ! H.class_ "column" $ do
          H.div ! H.id "solution" $ ""
        H.div ! H.id "scores" $ ""
