-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

module Fa.Uri where

import qualified Data.Text as T
import qualified Network.URI as U
import qualified Data.Csv as CSV

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Aeson (ToJSON, toJSON)
import System.FilePath.Posix (takeFileName)

instance CSV.ToField U.URI where
  toField = CSV.toField . uriString

instance ToJSON U.URI where
  toJSON = toJSON . uriString

-- does not handle query or fragments
escapeComponents :: String -> String
escapeComponents = intercalate "/" . unpackEscape . splitOn "/"
  where
    unpackEscape :: [String] -> [String]
    -- scheme / / authority / components...
    unpackEscape (scheme : "" : authority : components) =
      scheme : ("" :: String) : authority : map escapeComponent components
    unpackEscape components = map escapeComponent components

    escapeComponent :: String -> String
    escapeComponent = U.escapeURIString U.isUnescapedInURIComponent

canonicaliseUri :: U.URI -> T.Text -> Maybe U.URI
canonicaliseUri baseUri relativeRef =
  (`U.relativeTo` baseUri) <$> parseEscapeRelRef (T.unpack relativeRef)
  where
    parseEscapeRelRef :: String -> Maybe U.URI
    parseEscapeRelRef = U.parseRelativeReference . escapeComponents

uriString :: U.URI -> String
uriString = flip (U.uriToString id) ""

uriFileName :: U.URI -> FilePath
uriFileName = takeFileName . U.uriPath

uriHostName :: U.URI -> String
uriHostName U.URI { uriAuthority = Just U.URIAuth { uriRegName } } = uriRegName
uriHostName _ = ""
