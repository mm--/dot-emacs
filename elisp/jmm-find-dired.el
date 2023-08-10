;;; jmm-find-dired.el --- find-dired-with-command examples  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Joshua Moller-Mara

;; Author: Joshua Moller-Mara <jmm@cns.nyu.edu>
;; Keywords: files

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Useful examples of the new `find-dired-with-command' function.

;; - `git-annex-find-dired' is used to find matching files in a git
;;   annex directory.  For example, you could look for "--in=here"
;;   files to quickly see a sparse tree of only available files.  Or
;;   you could look for "--not --copies=2" to find things that need to
;;   be backed up.
;;
;; - `locate-dired' is like a faster `find-dired' since it uses your
;;   locate index.  Use it to locate files of a certain pattern under
;;   a given directory.
;;
;; - `locate-find-dired' is like `locate-dired', but additionally
;;   searches located files and only keeps those whose contents match
;;   some pattern.

;;; Code:
(require 'find-dired)

(defvar jmm/locate-args nil)
(defvar jmm/locate-args-history nil)

(defvar jmm/git-annex-dired-args nil)
(defvar jmm/git-annex-dired-args-history nil)

;;;###autoload
(defun locate-dired (dir args)
  "Run locate, like `find-dired'.
Found entries will need to be children of DIR.
ARGS is a string of shell arguments passed directly to locate."
  (interactive (list (read-directory-name "Run find in directory: " nil "" t)
		     (read-string "Locate args: " jmm/locate-args
				  '(jmm/locate-args-history . 1))))
  (setq jmm/locate-args args                ; save for next interactive call
	args (concat "locate"
		     " " args " "
		     "-e -0"
		     " | "
		     "grep --null-data --line-buffered " (shell-quote-argument (expand-file-name dir)) " | "
		     "xargs -0 --no-run-if-empty ls -ldNh"))
  (find-dired-with-command dir args))

;;;###autoload
(defun locate-ripgrep-dired (dir args pattern)
  "Like `locate-dired', but also searches contents with ripgrep.
DIR is a directory that located files must be under.
ARGS is passed to locate.
PATTERN is passed to ripgrep."
  (interactive (list (read-directory-name "Run find in directory: " nil "" t)
		     (read-string "Locate args: " jmm/locate-args
				  '(jmm/locate-args-history . 1))
		     (read-string "Ripgrep pattern: ")))
  (setq jmm/locate-args args                ; save for next interactive call
	args (concat "locate"
		     " " args " "
		     "-e -0"
		     " | "
		     "grep --null-data --line-buffered " (shell-quote-argument (expand-file-name dir)) " | "
		     "xargs -0 --no-run-if-empty rg --files-with-matches --no-messages " (shell-quote-argument pattern) " | "
		     "xargs -d '\n' --no-run-if-empty ls -ldNh"))
  (find-dired-with-command dir args))

;;;###autoload
(defun git-annex-find-dired (dir args)
  "Find files in git annex matching some git-annex-matching-options.
For example, you could use it to quickly list files --in=here."
  (interactive (list (read-directory-name "Run find in directory: " nil nil t)
		     (read-string "Git annex matching args: " jmm/git-annex-dired-args
				  '(jmm/git-annex-dired-args-history . 1))))
  (setq jmm/git-annex-dired-args args                ; save for next interactive call
	args (concat "git annex find --print0"
		     " " args
		     " " (shell-quote-argument (expand-file-name dir))
		     " | "
		     "xargs -0 --no-run-if-empty ls -ldNh"))
  (find-dired-with-command dir args))

(provide 'jmm-find-dired)
;;; jmm-find-dired.el ends here
