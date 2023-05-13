Here's a list of issues with this project, in no particular order.
Patches welcome :)

* Readme on github: the second paragraphs in definition lists are showing as
  code blocs on github. Should find a way to make both github and pandoc happy
  at the same time to have a proper web readme and a `man`page.

* Useless devel doc in Flake: `nix run github:Notkea/fa-tools` downloads the
  __developer__ documentation of the Haskell dependencies, for no good reason.

* Orphan instances for URI: the code should define its own wrapper type and
  define the instances on it, instead of relying on orphan instances on lib
  types.

* Submission pages with invalid encoding (non-UTF8 characters) seem to break
  the `fa-tools` subcommand. This seems to be due to FA embedding story
  previews with some other encoding within pages using the utf-8 charset.
