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
;; Activate it using something like:
;;
;; (setf (alist-get 'command completion-category-overrides)
;;       `((styles basic substringMX)))
;;
;; Note: This is really hacky, and will only work if
;;       `suggest-key-bindings' is `t'. Otherwise M-X's completion
;;       table won't return a "metadata" with a category of "command".


;;; Code:

;; TODO: Look up correct ways for optional dependencies
(require 'orderless nil t)

;;;###autoload
(defun completion-substringMX-try-completion (string table pred point)
  "A hacky way of only doing substring completion for `execute-extended-command-for-buffer'."
  (when (let ((case-fold-search nil))
	  ;; I can't really find a better way to determine if we're
	  ;; calling `execute-extended-command-for-buffer'
	  ;; Maybe we can advise the function and use `minibuffer-with-setup-hook'
	  (string-match-p "M-X" (minibuffer-prompt)))
    (completion-substring-try-completion string table pred point)))

;;;###autoload
(defun completion-substringMX-all-completions (string table pred point)
  "See `completion-substringMX-try-completion'."
  (when (let ((case-fold-search nil))
	  (string-match-p "M-X" (minibuffer-prompt)))
    (completion-substring-all-completions string table pred point)))

;;;###autoload
(defun completion-orderlessMX-try-completion (string table pred point)
  "A hacky way of only doing orderless completion for `execute-extended-command-for-buffer'."
  (when (let ((case-fold-search nil))
	  (string-match-p "M-X" (minibuffer-prompt)))
    (orderless-try-completion string table pred point)))

;;;###autoload
(defun completion-orderlessMX-all-completions (string table pred point)
  "See `completion-orderlessMX-try-completion'."
  (when (let ((case-fold-search nil))
	  (string-match-p "M-X" (minibuffer-prompt)))
    (orderless-all-completions string table pred point)))

;;;###autoload
(add-to-list 'completion-styles-alist
             '(substringMX
	       completion-substringMX-try-completion completion-substringMX-all-completions
	       "Substring completion, but only for `execute-extended-command-for-buffer' (a.k.a. \"M-X\")."))

;;;###autoload
(add-to-list 'completion-styles-alist
             '(orderlessMX
	       completion-orderlessMX-try-completion completion-orderlessMX-all-completions
	       "Orderless completion, but only for `execute-extended-command-for-buffer' (a.k.a. \"M-X\")."))

(provide 'completion-jmm)
;;; completion-jmm.el ends here
