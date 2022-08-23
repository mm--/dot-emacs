;;; jmm-zsh.el --- Zsh history completions           -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Joshua Moller-Mara

;; Author: Joshua Moller-Mara <jmm@cns.nyu.edu>
;; Keywords: processes

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

;; Lets you use your Zsh history from inside Emacs.
;;
;; `zsh-run-history' is like `shell-command', but uses your zsh history for completion.
;;
;; `zsh-insert-history-value' is for Eshell or shell.  It prompts for
;; some zsh history value and inserts it at point.

;; "orderless" is a good completion style to use.
;; If you have it, you might want to run the following:
;; (setf (alist-get 'jmm-zsh-history completion-category-overrides)
;;       '((styles orderless)))

;;; Code:

;; -*- lexical-binding: t; -*-
(defvar jmm-zsh-histfile "~/.config/zsh/history"
  "The path to the file containing Zsh history.")

(defvar jmm-zsh-history-list nil
  "Contains zsh history for `completing-read', but gets overwritten.")

(defun jmm-zsh-get-history ()
  "Return a list of Zsh history"
  (let (hist)
    (with-temp-buffer
      (insert-file-contents-literally jmm-zsh-histfile)
      (goto-char (point-min))
      (while (re-search-forward (rx bol ": " (1+ digit) ":" (1+ digit) ";" (group-n 1 (1+ any)) eol) nil t)
	(push (match-string-no-properties 1) hist)))
    hist))

(defun jmm-zsh-history-completions ()
  "Return a completion table of zsh history."
  (let* ((collection (jmm-zsh-get-history))
	 (sorting (lambda (strings)
		    (seq-sort-by (lambda (e)
				   (or (seq-position collection e) most-positive-fixnum))
				 #'< strings))))
    (lambda (string pred action)
      (if (eq action 'metadata)
	  `(metadata (category . jmm-zsh-history)
		     (display-sort-function . ,sorting)
		     )
	(complete-with-action action collection string pred)))))

;;;###autoload
(add-to-list 'completion-category-defaults
	     '(jmm-zsh-history (styles substring)))

;;;###autoload
(defun jmm-zsh-insert-history-value ()
  "Prompt for some Zsh history value, insert it."
  (interactive nil eshell-mode shell-mode)
  (let* ((collection (jmm-zsh-history-completions)))
    (setq jmm-zsh-history-list (funcall collection "" nil t))
    (insert (completing-read "Zsh: " collection
			     nil t nil
			     'jmm-zsh-history-list))))
;;;###autoload
(defalias 'zsh-insert-history-value #'jmm-zsh-insert-history-value)

;;;###autoload
(defun zsh-run-history ()
  "Run a `shell-command', using Zsh history for completion."
  (interactive)
  (let* ((collection (jmm-zsh-history-completions)))
    (setq jmm-zsh-history-list (funcall collection "" nil t))
    (shell-command (completing-read "Zsh: " collection
				    nil nil nil
				    'jmm-zsh-history-list))))

(provide 'jmm-zsh)
;;; jmm-zsh.el ends here
