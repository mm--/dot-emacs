# Josh's Emacs Config

Hey there! This repository contains my Emacs configuration file (see
[`jmm-emacs.org`](./jmm-emacs.org)). My org-mode configuration is located in [`jmm-org-config.org`](./jmm-org-config.org)

Most of the documentation is included in the Emacs configuration itself, it's much like Sacha Chua's Emacs configuration.

This repo is not probably all that portable, so it may not work on
your computer. It's mostly here so you can take the bits and pieces
you find useful.

Some things that may be useful
- `josh/helm-org-clock-in` uses helm to quickly clock into Org-mode tasks
- `josh/helm-org-jump` uses helm to quickly jump to an Org-mode entry
- `start-xterm` quickly launches a new terminal, even ssh-ing into
  other machines if you're using tramp. ~~(Sorry, I don't like using
  shell, eshell, or term)~~ Actually I'm kinda really starting to like
  `eshell` and I've used `ansi-term` through `shell-pop` for a while.
  - Some `eshell` utilities like:
	- Making `ls` items clickable (either inserting the filename directly into the prompt or visiting the file at point)
	- Quickly changing directories to frequently used ones using `fasd` and `ivy` (see `jmm/counsel-fasd-directories-eshell`)
	- Quickly prompting `eshell` history with `ivy` (See `jmm/eshell-prompt-history`)
- Lots of [git-annex](https://git-annex.branchable.com/) utilities such as:
  - Quickly adding metadata with the keybinding `@ t` (See `jmm/dired-git-annex-tag`)
  - Marking `git-annex` files `dired` that match `git-annex-matching-options` (such as `--not --copies=2` for marking files with only one copy). See `jmm/dired-mark-files-git-annex-matching`.
  - Showing `git-annex` file sizes (not symlink sizes!) in `dired`. (See `jmm/dired-git-annex-add-real-file-sizes`)
- Various `notmuch` utilities
  - Washing out tracking parameters and tracking/munged links in `text/plain` emails. (See `jmm/notmuch-wash-decodable-links` and `jmm/notmuch-wash-superfluous-parameters`)

## Cangjie 5 ##

I've also created a more complete
[Cangjie 5](https://en.wikipedia.org/wiki/Cangjie_input_method) input
method in the `elisp/cangjie5.el` file. It's based off of
[libcangjie](https://github.com/Cangjians/libcangjie). This Cangjie
method, compared to two built-in ones in Emacs (`chinese-cns-tsangchi` and `chinese-b5-tsangchi`) has the following
features:
- Cangjie version 5 support (instead of version 3)
- Both simplified and traditional characters can be input
- Hitting `<space>` accepts input (a bit easier than `C-<space>`)
- Disambiguation of characters using "x". For example, "a" is "日" and "xa" is "曰" (or you can use "a" and then press "2")

