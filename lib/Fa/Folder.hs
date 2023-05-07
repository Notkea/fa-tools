-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

module Fa.Folder where

import qualified Data.Text as T

import Network.URI (URI)

data FolderEntry = FolderEntry
  { name :: T.Text
  , url :: URI
  } deriving (Show)
