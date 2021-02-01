{-# language PatternSynonyms, MultiWayIf, LambdaCase, OverloadedStrings #-}
{-# language TypeOperators, DataKinds, TypeApplications, ViewPatterns #-}

module Gobble.Message where

import Network.WebSockets
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.UTF8 as B8

import Gobble.Core

data Status'Query = Who'Query | Word'List'Query

data Gobble'Message
  = Status'Message Status'Query
  | Submit'Message [T.Text]
  | Delete'Message T.Text
  | Like'Message T.Text
  | Chirp'Message T.Text
  | IDK'Message T.Text

parse'ws'message :: DataMessage -> Gobble'Message
parse'ws'message (Text t _) =
  let t' = T.pack $ B8.toString t
  in case T.words $ T.toUpper t' of
    "GOBBLE":ws -> Submit'Message ws
    "DOBBLE":w:[] -> Delete'Message w
    "WOBBLE":w:[] -> Like'Message w
    _ -> case T.stripPrefix "chirp " t' of
      Just chirp -> Chirp'Message chirp
      Nothing -> case t' of
        "who-else" -> Status'Message Who'Query
        "words" -> Status'Message Word'List'Query
        _ -> IDK'Message t'
parse'ws'message (Binary m) = IDK'Message $ T.pack $ B8.toString m
  
pattern Query cmd <- Text cmd _
pattern Words ws <- Text (T.words.T.toUpper.T.pack.B8.toString -> "GOBBLE":ws) _
pattern Delete w <- Text (T.words.T.toUpper.T.pack.B8.toString -> "DOBBLE":w:[]) _
pattern Wow'wow w <- Text (T.words.T.toUpper.T.pack.B8.toString -> "WOBBLE":w:[]) _
pattern Chirp msg <- Text (T.stripPrefix "chirp ".T.pack.B8.toString -> Just msg) _
