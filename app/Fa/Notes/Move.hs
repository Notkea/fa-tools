-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

module Fa.Notes.Move where

import qualified Network.HTTP.Client as HTTP
import qualified Fa.Notes.Note as FAN

move :: HTTP.Manager -> Int -> FAN.NoteTargetFolder -> IO ()
move = FAN.moveNote
