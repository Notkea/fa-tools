---
title: FA-NOTES(1) fa-tools
section: 1
header: fa-tools
date: 2023-05
---


# NAME

fa-notes - list and retrieve notes from FurAffinity


# ENVIRONMENT VARIABLES

`FA_SESSION_HEADERS`
:   Session headers extracted from an existing authenticated browser session.
    Given as a dictionary of key and values separated by `: `, one per line.
    Required.

    Those request headers can be copied from a web browser by opening the
    "developer tools", going to the "network" tab, then the "request" sub-tab.


# ACTIONS

`fa-notes list`
:   List notes from the main inbox as CSV.

    Columns are: identifier, sender, date, unread, subject.

`fa-notes read NOTE_ID`
:   Retrieve and print a note's info as JSON.

    This marks the note as "read" for both the sender and the recipient.

    Fields are: sender, recipient, date, subject, content.

`fa-notes move NOTE_ID [unread|archive|trash]`
:   Move the designated note to the target folder.


# KNOWN ISSUES

The following issues affecting the program are known. Patches welcome.

* Only the notes from the main folder are listed.
* Note priorities are not handled.
* The subject returned by `fa-notes read` always contains the prefix "RE:",
  regardless of whether it is an actual reply. This is as displayed by FA's
  interface.


# EXAMPLE

The following snippet finds new notes in the inbox and sends them via email to
some recipient. This can act as a replacement for the missing note email
notification feature of the site.

Fish shell commands:

```fish
# Get into a shell with the fa-tools, q, jq programs
nix shell github:Notkea/fa-tools 'nixpkgs#'{q-text-as-data,jq}

# Use an existing FA user session, to extract from a web browser
set --export FA_SESSION_HEADERS "
User-Agent: [...]
Cookie: [...]
[...]
"

# Retrieve the last sent note ID, or 0 if we don't know any yet
set last_sent_id (cat last_sent_id.txt || echo 0)

# List the current notes and keep only the new IDs
set new_notes_ids (fa-notes list | q -d, -H "
  select distinct(identifier)
  from -
  where identifier > $last_sent_id
  order by identifier asc
")

for id in $new_notes_ids
  # Fetch the note's data
  set json (fa-notes read $id)

  # Extract and format note fields
  set sender    (echo "$json" | jq -r .sender | xargs basename)
  set recipient (echo "$json" | jq -r .recipient | xargs basename)
  set datetime  (echo "$json" | jq -r .date | date -f - --rfc-email)
  set subject   (echo "$json" | jq -r .subject | head -n1)
  set content   (echo "$json" | jq -r .content | string collect)

  # Format and send the email
  echo "\
    Content-Type: text/plain; charset="utf-8"
    From: $sender on FurAffinity.net <notes.relay@not.really.furaffinity.net>
    To: $recipient <actual.recipient@some.host>
    Date: $datetime
    Subject: $subject

    $content
  " | string trim | sendmail --read-recipients

  # Save the note ID, so that we do not send it again next time
  echo $id > last_sent_id.txt
end
```


# SEE ALSO

__fa-tools(7)__, __fa-subs(1)__


# LICENCE AND COPYRIGHT

Copyright (C) 2021-2023 Notkea.

_fa-tools_ is distributed under the terms of European Union Public Licence
version 1.2, a copy of which is provided in `licence.txt`.
