---
title: FA-TOOLS(7) fa-tools
date: 2023-05
---


# NAME

fa-tools - a CLI toolbox to retrieve content from FurAffinity


# NOTES

The program is intended for personal use.
Mass scrapping requires an explicit authorisation from the staff of the
website.

FurAffinity does not expose a public API, so the program relies on scrapping
content from some user browsing session.

The scrapping program assumes that the session uses the "Modern" theme on the
website.


# ENVIRONMENT VARIABLES

`FA_SESSION_HEADERS`
:   Session headers extracted from an existing authenticated browser session.
    Given as a dictionary of key and values separated by `: `, one per line.
    Required.

    Those request headers can be copied from a web browser by opening the
    "developer tools", going to the "network" tab, then the "request" sub-tab.


# COMMANDS

`fa-subs`
:   List and retrieve gallery submissions. See __fa-subs(1)__.


# DEVELOPMENT

The source code is available at <https://github.com/Notkea/fa-tools>.

To start a development environment:

```fish
# Enter the nix development shell with all the tools made available
nix develop

# Generate the cabal project file from the hpack package.yaml
hpack

# Build using Cabal
cabal build

# The library documentation can be accessed and searched through Hoogle
hoogle serve --local

# An editor using the LSP haskell-language-server should recognised everything
```


# SEE ALSO

__fa-subs(1)__


# LICENCE AND COPYRIGHT

Copyright (C) 2021-2023 Notkea.

_fa-tools_ is distributed under the terms of European Union Public Licence
version 1.2, a copy of which is provided in `licence.txt`.
