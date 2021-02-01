{-# language LambdaCase, OverloadedStrings #-}

module Gobble.Message where

import Network.WebSockets
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy.UTF8 as B8

import Gobble.Core

data Status'Query = Who'Query | Word'List'Query
  deriving (Show)
-- besides Chirp, messages (requests mostly) that the client sends to
-- update state
data Gobble'Message
  = Status'Message Status'Query
  | Submit'Message [T.Text]
  | Delete'Message T.Text
  | Like'Message T.Text
  | Chirp'Message Chirp
  | IDK'Message T.Text
  deriving (Show)

data Chirp
  = Help'Me
  | Define T.Text
  | Who's'Gotten T.Text
  | Chirp T.Text
  deriving (Show)

-- should use real parser, no?
parse'ws'message :: DataMessage -> Gobble'Message
parse'ws'message (Text t _) =
  let t' = T.pack $ B8.toString t
  in case T.words $ T.toUpper t' of
    "GOBBLE":ws -> Submit'Message ws
    "DOBBLE":w:[] -> Delete'Message w
    "WOBBLE":w:[] -> Like'Message w
    _ -> case T.stripPrefix "chirp " t' of
           Just msg -> case T.words msg of
             ["?def",w] -> Chirp'Message $ Define w
             ["?who",w] -> Chirp'Message $ Who's'Gotten w
             ["?help"] -> Chirp'Message $ Help'Me
             _ -> Chirp'Message $ Chirp msg
           Nothing -> case t' of
             "who-else" -> Status'Message Who'Query
             "words" -> Status'Message Word'List'Query
             _ -> IDK'Message t'
parse'ws'message (Binary m) = IDK'Message $ T.pack $ B8.toString m
