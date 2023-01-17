;;; jmm-mark.el --- Utilities for dealing with the mark  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Joshua Moller-Mara

;; Author: Joshua Moller-Mara <jmm@cns.nyu.edu>

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

;; I learned Emacs before `transient-mark-mode' was set to default, so
;; there are some things I miss about just using the mark as a mark.
;; I now set `transient-mark-mode' off, but there are some situations
;; where I'd like it turned on.

;; Try `jmm-mark-mode'.

;;; Code:
;; (defun jmm-mark--setup-advice (&optional remove))
(defgroup jmm-mark nil
  "JMM mark utilities"
  :prefix "jmm-mark-")

(defcustom jmm-mark-after-functions
  '((mark-word . jmm-activate-mark-advice)
    (rectangle-mark-mode . jmm-mark-activate-rectangle-advice))
  "Functions after which `transient-mark-mode' is turned on in `jmm-mark-mode'.
Takes a cons of functions and advice to use for them."
  :type '(repeat (cons function function))
  ;; :set (lambda (symbol value)
  ;; 	 (jmm-mark--setup-advice t)
  ;; 	 (set-default symbol value)
  ;; 	 (jmm-mark--setup-advice jmm-mark-mode))
  )

;;;###autoload
(defun jmm-mark-activate-toggle ()
  "Activates mark or toggles it.
The first time it will unconditionally activate the mark.
Subsequent invocations will toggle the between deactivating and activating the mark."
  (interactive)
  (if (eq last-command this-command)
      (if (region-active-p)
	  (progn
	    (deactivate-mark t)
	    (if (eq transient-mark-mode 'lambda)
		(kill-local-variable 'transient-mark-mode)))
	(activate-mark))
    (activate-mark)))

;;;###autoload
(defun jmm-activate-mark-advice (&rest args)
  "Activate mark if `transient-mark-mode' is nil.
Basically used as :after advice to make some commands act like they do
when `transient-mark-mode' is on."
  (unless transient-mark-mode
    (activate-mark)))

(defun jmm-mark-activate-rectangle-advice (&rest args)
  "Activate transient mark for `rectangle-mark-mode'.
Run after `rectangle-mark-mode'."
  (when rectangle-mark-mode
    (unless transient-mark-mode
      (activate-mark))))

(defun jmm-mark--setup-advice (&optional remove)
  "Turn on `transient-mark-mode' after functions in `jmm-mark-after-functions'.
(Or turn off if REMOVE is non-nil)."
  (if remove
      (cl-loop for (sym . adv) in jmm-mark-after-functions
	       do (advice-remove sym adv))
    (cl-loop for (sym . adv) in jmm-mark-after-functions
	     do (advice-add sym :after adv))))

(defvar-keymap jmm-mark-mode-map
  :doc "Keymap for `jmm-mark-mode'."
  "C-S-SPC" #'jmm-mark-activate-toggle)

;; FIXME: Autoloads seem to mess things up when defining a global minor mode.
;; But without an autoload for the variable, we can't theme it

;;;###autoload(autoload 'jmm-mark-mode "jmm-mark" nil t)

;;;###autoload
(define-minor-mode jmm-mark-mode
  "A minor mode for using `transient-mark-mode' when it's disabled.
`transient-mark-mode' will be enabled after:
 - Marking a word with `mark-word'."
  :lighter nil
  :global t
  :keymap jmm-mark-mode-map
  (jmm-mark--setup-advice (not jmm-mark-mode)))

(provide 'jmm-mark)
;;; jmm-mark.el ends here
