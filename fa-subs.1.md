---
title: FA-SUBS(1) fa-tools
section: 1
header: fa-tools
date: 2023-05
---


# NAME

fa-subs - list and retrieve submissions from FurAffinity


# ENVIRONMENT VARIABLES

`FA_SESSION_HEADERS`
:   Session headers extracted from an existing authenticated browser session.
    Given as a dictionary of key and values separated by `: `, one per line.
    Required.

    Those request headers can be copied from a web browser by opening the
    "developer tools", going to the "network" tab, then the "request" sub-tab.


# ACTIONS

`fa-subs list [OPTIONS] LIST_PAGE_URL`
:   Retrieve and list submissions from a gallery, scraps, or user favourites
    as CSV.

    Columns are: page, user, thumbnail, kind, rating, title.

    `-a` `--all-folders`
    :   List items from all folders (default: false)

`fa-subs info SUBMISSION_PAGE_URL`
:   Retrieve and print a submission's info as JSON.

    Fields are: page, download, author, date, tags, folders(name, url), title,
    description.

`fa-subs download [OPTIONS] SUBMISSION_URL`
:   Download a submission file from the given page or direct link.
    Prints the name of the output file once downloaded.

    `-o` `--output=FILE`
    :   Output file (default: original name)


# EXAMPLE

The following snippet lists all the submissions in a gallery folder, download
all the writings and convert them to EPUB e-book to be read offline on an e-ink
tablet for example.

Fish shell commands:

```fish
# Get into a shell with the fa-tools, q, jq and ebook-convert programs
nix shell github:Notkea/fa-tools 'nixpkgs#'{q-text-as-data,jq,calibre}

# Use an existing FA user session, to extract from a web browser
set --export FA_SESSION_HEADERS "
User-Agent: [...]
Cookie: [...]
[...]
"

# Retrieve the list of submissions in a gallery folder,
# and filter it to keep the page URLs of only the text submissions
set text_submission_pages (
  fa-subs list https://www.furaffinity.net/gallery/$SOMEUSER/ |
    q -d, -H "select distinct(page) from - where kind = 't-text'"
)

# For each text submission...
for submission_page in $text_submission_pages
  # Retrieve submission metadata, unpack JSON fields into shell variables
  fa-subs info "$submission_page" |
    jq -r '.download, .author, .date, .title, (.tags | join(","))' |
    read -L download_url author pubdate title tags

  # Download the submission file
  set filename (fa-subs download "$download_url")

  # Convert to an EPUB e-book, adding metadata from the submission page
  ebook-convert "$filename" "$filename.epub" \
    --authors "$author" \
    --pubdate "$pubdate" \
    --title "$title" \
    --tags "$tags" \
    --no-default-epub-cover
end
```


# KNOWN ISSUES

The following issues affecting the program are known. Patches welcome.

* The program requires the session headers environment variable to be set even
  for guest sessions (not using an account). This should be relaxed.

* The `list` action enumerates pages without any cooldown. This might be
  necessary when enumerating a large number of pages to avoid being
  rate-limited.

* The `list` action only works for galleries, scraps, and user favourites,
  but not for listing the aggregated new submissions from watched accounts.


# SEE ALSO

__fa-tools(7)__, __fa-notes(1)__


# LICENCE AND COPYRIGHT

Copyright (C) 2021-2023 Notkea.

_fa-tools_ is distributed under the terms of European Union Public Licence
version 1.2, a copy of which is provided in `licence.txt`.
