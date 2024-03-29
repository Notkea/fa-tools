-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

module Fa.Submissions.Folder where

import qualified Data.Text as T

import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Network.URI (URI)
import Fa.Uri ()

data FolderEntry = FolderEntry
  { name :: T.Text
  , url :: URI
  } deriving (Generic, Show, ToJSON)
