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


# SEE ALSO

__fa-tools(7)__, __fa-subs(1)__


# LICENCE AND COPYRIGHT

Copyright (C) 2021-2023 Notkea.

_fa-tools_ is distributed under the terms of European Union Public Licence
version 1.2, a copy of which is provided in `licence.txt`.
