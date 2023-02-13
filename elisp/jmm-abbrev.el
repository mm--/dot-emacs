;;; jmm-abbrev.el --- Josh's extra abbrev utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Joshua Moller-Mara

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

;; These just set up some extra abbreviations I sometimes use.

;; Not sure if I should use autoloads or just require this entire file.
;; Pros for not doing autoload:  You can reload this file.

;;; Code:

;;; Modes where I use abbrevs

(cl-loop for mode in '(nxml-mode-hook
		       css-mode-hook
		       text-mode-hook
		       eshell-mode-hook
		       emacs-lisp-mode-hook)
	 do
	 (add-hook mode #'abbrev-mode))


;; Global abbrevs

;; Original way of specifying abbrevs: ;;;###autoload(define-abbrev global-abbrev-table "0dd" "" 'jmm-abbrev-insert-current-date :system t)

;;;###autoload
(defun jmm-abbrev-insert-current-date ()
  "An abbreviation hook to insert current date."
  (insert (format-time-string "%Y-%m-%d")))
(define-abbrev global-abbrev-table "0dd" "" 'jmm-abbrev-insert-current-date :system t)

;;;###autoload
(defun jmm-abbrev-insert-HH-MM ()
  "An abbreviation hook to insert current time."
  (insert (format-time-string "%02H:%02M")))
(define-abbrev global-abbrev-table "0hm" "" 'jmm-abbrev-insert-HH-MM :system t)

;;;###autoload
(defun jmm-abbrev-insert-HH-MM-SS ()
  "An abbreviation hook to insert current time."
  (insert (format-time-string "%02H:%02M:%02S")))
(define-abbrev global-abbrev-table "0hms" "" 'jmm-abbrev-insert-HH-MM-SS :system t)

;;;###autoload
(defun jmm-abbrev-insert-date-time ()
  "An abbreviation hook to insert current date and time."
  (insert (format-time-string "%Y-%m-%d %02H:%02M")))
(define-abbrev global-abbrev-table "0dhm" "" 'jmm-abbrev-insert-date-time :system t)

(provide 'jmm-abbrev)
;;; jmm-abbrev.el ends here
