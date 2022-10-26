;;; completion-jmm.el --- Personal utilities for completions  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Joshua Moller-Mara

;; Author: Joshua Moller-Mara <jmm@cns.nyu.edu>
;; Keywords: convenience

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

;; Allows you to use "substring" style completion, but only for
;; `execute-extended-command-for-buffer'.
;; To do this, it creates a new completion category 'command-for-buffer.
;;
;; Activate it using:
;;
;; (require 'completion-jmm)
;; (advice-add 'execute-extended-command-for-buffer :around #'jmm-advice-M-X-override-completion)
;;
;; and set the completion style using:
;;
;; (setf (alist-get 'command-for-buffer completion-category-overrides)
;;       `((styles basic substring)))
;;

;; FIXME: This file needs to be required. Simply using autoloads doesn't work.
;; It might have something to do with the way
;; `advice-eval-interactive-spec' uses `eval' on the interactive part of the advice.

;;; Code:

(defun jmm-advice--M-X-override-completion-metadata (orig string pred action)
  "This is advice that overrides a completion table.
It will return a custom metadata setting with the category 'command-for-buffer."
  (if (eq action 'metadata)
      '(metadata
	(affixation-function . read-extended-command--affixation)
	(category . command-for-buffer))
    (funcall orig string pred action)))

;; FIXME: This isn't actually a command, and should be excluded from
;; being listed in M-x.

;;;###autoload (autoload 'jmm-advice-M-X-override-completion "completion-jmm")
(defun jmm-advice-M-X-override-completion (orig &rest args)
  "Sets a custom completion category for `execute-extended-command-for-buffer'.
The new completion category becomes 'command-for-buffer.
You can set styles for \\[execute-extended-command-for-buffer] using, for example:

 (setf (alist-get 'command-for-buffer completion-category-overrides)
       `((styles basic orderless)))

This needs to be applied using:

 (advice-add 'execute-extended-command-for-buffer :around #'jmm-advice-M-X-override-completion)
"
  (interactive
   (lambda (spec)
     (minibuffer-with-setup-hook
	 (:append (lambda () (add-function :around (local 'minibuffer-completion-table) #'jmm-advice--M-X-override-completion-metadata)))
       (advice-eval-interactive-spec spec))))
  (apply orig args))

(provide 'completion-jmm)
;;; completion-jmm.el ends here
