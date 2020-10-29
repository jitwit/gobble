{-# language OverloadedStrings, ImplicitParams, TemplateHaskell, LambdaCase #-}
{-# language FlexibleContexts #-}

module Gobble.Render where

import Control.Monad
import Control.Lens
import Data.Set.Lens
import qualified Diagrams.Prelude as D
import Diagrams.Prelude (Diagram,V2 (..))
import Diagrams.Backend.SVG
import qualified Data.Text as T
import qualified Data.Text.Internal.Lazy as TLI
import Data.Time.LocalTime
import Data.Time.Format
import Data.Text (Text)
import Data.Colour.SRGB
import Text.Blaze.Html5 as H hiding (map,main,head,style)
import qualified Text.Blaze.Html5 as H (head)
import Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5.Attributes as H hiding (form,span)
import qualified Data.Aeson as A
import qualified Data.Map as M
import Data.Map (Map)
import Data.List
import Data.Function

import Gobble.Core

type H = TLI.Text

data WordResult = Unique Text | Shared Text | Mistake Text | Gaffe Text | Idk'Oops Text
data RoundResult = RoundResult (Map Text Text) (Map Text Int)

word'result :: Text -> Int -> WordResult
word'result w n | n == 1  = Unique w
                | n > 1   = Shared w
                | n == -1 = Mistake w
                | n < -1  = Gaffe w
                | otherwise = Idk'Oops w

classify'words :: RoundResult -> Map Text WordResult
classify'words = \case
  RoundResult sol subs ->
    let misses = negate <$> subs .- sol
        hits   = subs .* sol
     in M.mapWithKey word'result (misses .+ hits)

instance ToMarkup WordResult where
  toMarkup = \case
    Unique w   -> H.div $ H.text w
    Shared w   -> H.div ! H.style "color:#969696;" $ H.text w
    Mistake w  -> H.div ! H.style "color:#FE160E;" $ H.text w
    Gaffe w    -> H.div ! H.style "color:#EE8509;" $ H.text w
    Idk'Oops w -> H.div ! H.style "color:#854B21;" $ H.text w

instance ToMarkup Word'List'View where
  toMarkup (Word'List'View gob) = 
    let sol = gob ^. board.word'list
        wl = sortBy (flip compare `on` T.length . fst) $ M.toList sol
        gotten = gob & setOf (players.folded.answers.to M.keys.folded)
     in do h3 "word list"
           table $ forM_ wl $ \(w,d) ->
             tr $ do td $ toMarkup $ if gotten^.contains w
                       then Shared w else Unique w
                     td (H.text d)

instance ToMarkup Player'Status where
  toMarkup (Player'Status who act) = case act of
    Here -> H.text who
    There -> H.text $ "(" <> who <> " 💤)"
    Elsewhere -> H.text $ "{" <> who <> " 👻}"

instance ToMarkup Chat'View where
  toMarkup (Chat'View gob) = table $ do
    thead $ mconcat $ intersperse (H.text ", ")
          [ toMarkup $ Player'Status who (plr ^. status)
          | (who,plr) <- gob^.players.to M.assocs ]
    gob & iforMOf_ (chat'room.messages.ifolded) $ \t (Chat'Message tweet author) ->
      tr $ do td $ H.div ! H.class_ "occurrence" $ do
                H.span ! H.class_ "happening" $ H.string $ fmt t
                H.text author
              td $ H.text tweet
      where loc = defaultTimeLocale
            zon = hoursToTimeZone (-4)
            fmt = formatTime loc " %H:%M:%S" . utcToZonedTime zon

instance ToMarkup Score'View where
  toMarkup (Score'View gob) = report where
    sol = gob ^. board.word'list
    subs = gob^..players.traversed.answers
    res = classify'words $ RoundResult sol $ M.unionsWith (+) subs
    report = table $ do
      thead $ do td $ ""
                 gob & mapMOf_ (players.to M.keys.folded) (th.H.text)
      tr $ do
        td "score"
        gob & forMOf_ (players.traversed.score) $ \n ->
          td ! H.style "text-align:center;" $ H.text $ T.pack $ show n
      tr $ do
        let r = ((gob^.current'round)`mod`5)+1
        td $ H.string ("total (" <> (show r) <> "/5)")
        gob & forMOf_ (players.traversed.total'score) $ \n ->
          td ! H.style "text-align:center;" $ H.text $ T.pack $ show n
      when (1 < length subs) $
        tr $ do td "solo"
                gob & forMOf_ (players.traversed.solo'score) $ \n ->
                  td ! H.style "text-align:center;" $ H.text $ T.pack $ show n
      tr $ do td "words"
              forM_ (M.keys <$> subs) $ \ws ->
                td $ ul $ mconcat [ toMarkup $ res M.! w | w <- ws ]

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
render'chat = renderHtml . toMarkup . Chat'View

render'solution :: Gobble -> H
render'solution = renderHtml . toMarkup . Word'List'View

render'scores :: Gobble -> H
render'scores = renderHtml . toMarkup . Score'View

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
      link ! H.rel "stylesheet" ! H.href "static/gobble.css?13"
      link ! H.rel "icon" ! H.href "static/icon.png"
      script ! H.src "static/jquery-3.4.1.slim.js" $ ""
      script ! H.src "static/gobble.js?13" $ ""
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
