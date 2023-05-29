-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

module Fa.Notes.Main where

import qualified Network.HTTP.Client as HTTP

import qualified Fa.Notes.List as APP
import qualified Fa.Notes.Read as APP

import qualified Fa.Notes.Note as FAN

import Fa.Common
import System.Console.CmdArgs

data Options
  = List
  | Read
      { identifier :: Int
      }
  | Move
      { identifier :: Int
      , folder :: FAN.NoteTargetFolder
      }
  deriving (Show, Data, Typeable)

optionsModes :: Options
optionsModes = modes
  [ List
      &= help "List notes from the main inbox as CSV."
  , Read
      { identifier = def
          &= typ "NOTE_ID"
          &= argPos 0
      }
      &= help "Retrieve and print a note's info as JSON."
  , Move
      { identifier = def
          &= typ "NOTE_ID_TO_MOVE"
          &= argPos 0
      , folder = FAN.Unread
          &= typ "[unread|archive|trash]"
          &= argPos 1
      }
      &= help "Move the designated note to the target folder."
  ]
  &= summary "List and retrieve notes from FurAffinity."
  &= program "fa-subs"
  &= help (unlines envHelp)
  &= helpArg [explicit, name "h", name "help"]
  &= versionArg [explicit, name "version"]

main :: IO ()
main = do
  arguments <- cmdArgs optionsModes
  client <- initHttpManager
  run client arguments

run :: HTTP.Manager -> Options -> IO ()
run client List = APP.list client
run client Read { identifier } = APP.read client identifier
run client Move { identifier, folder } = FAN.moveNote client identifier folder
