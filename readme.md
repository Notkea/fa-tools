FurAffinity tools
=================

A collection of quick and dirty tools to download content from FurAffinity.

Note that the programs are intended for personal use.
Mass scrapping requires an explicit authorisation from the staff of the website.


Development
-----------

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


Overview
--------

The following snippet can be used to export all writings listed in a gallery
to EPUB e-books. It makes use of:

* `fish` as the shell,
* `nix run` to run the scripts, taking care of their dependencies,
* `q` (`q-text-as-data`) to filter the CSV listing with an SQL query,
* `pandoc` to generate an EPUB e-book from the downloaded Markdown.

```fish
set --export FA_BROWSER_HEADERS "headers extracted from browser here"
set --export FA_COOLDOWN_SEC 1

nix run github:notkea/fa-scripts#list-submissions \
  https://www.furaffinity.net/gallery/FA_USERNAME/ \
  > subs.csv

for s in (q -d, -H "select distinct(url) from subs.csv where type = 't-text'")
  nix run github:notkea/fa-scripts#get-submission-markdown $s > (basename $s).md
  pandoc -i (basename $s).md -o (basename $s).epub
  sleep $FA_COOLDOWN_SEC
end
```


Scrapping
---------

FurAffinity does not expose a public API, so the scripts rely on scrapping
content from some user browsing session.

The scrapping scripts assume that the session uses the "Modern" theme on the
website.

The following environment variables are recognised and used in the various
scrapping scripts:

`FA_BROWSER_HEADERS`
: Headers extracted from an existing authenticated browser session.
  Given as a dictionary of key and values separated by `: `, one per line.

`FA_COOLDOWN_SEC`
: Minimum delay between HTTP requests within scripts.
  Does not affect separate calls of individual scripts.
  Default: 1 second.


Scripts
-------

The scripts are deliberately simple so that they can be used from the shell,
not just from Python programs or the Python REPL.

Data are exported as comma-separated values (CSV) files between scripts.
Output is simply written to `stdout`.

This allows easily plugging intermediary manual or automatic steps in a
download and processing pipeline.

Script list:

`list-submissions.py <gallery-root-url>`
: List all submissions in a gallery along with their type.
  Looks at all folders and in the "scraps" category.
  Fields: title, rating, type, url, thumbnail_url, folder_name, folder_url.

`get-submission-metadata.py <submission-url>`
: Retrieves metadata about a submission and prints those as a JSON on the
  standard output.

`get-submission-markdown.py <submission-url>`
: Gets either a text story submission or the description to generate an ebook.
  Adds the title, author, submission date, URL, tags and folders to the export.
  The Markdown output can then be fed to `pandoc` to generate an `.epub`.


Licence and copyright
---------------------

Copyright (C) 2021-2023 Notkea.

_fa-tools_ is distributed under the terms of European Union Public Licence
version 1.2, a copy of which is provided in `licence.txt`.
