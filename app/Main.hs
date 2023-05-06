-- File part of fa-tools
-- Copyright 2023 Notkea
-- Licensed under the EUPL version 1.2

import System.Console.CmdArgs

data Options
  = Dummy
  deriving (Show, Data, Typeable)

optionsModes :: Options
optionsModes = modes
  [ Dummy
  ]
  &= summary "A CLI toolbox to download content from FurAffinity."
  &= program "fa-tools"
  &= helpArg [explicit, name "h", name "help"]
  &= versionArg [explicit, name "version"]

main :: IO ()
main = do
  arguments <- cmdArgs optionsModes
  print arguments
