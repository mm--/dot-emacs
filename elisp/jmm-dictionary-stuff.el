;;; jmm-dictionary-stuff.el --- dictionary.el utilities    -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Joshua Moller-Mara

;; Author: Joshua Moller-Mara <jmm@cns.nyu.edu>
;; Keywords: wp

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

;; These are just functions to use specific dictionaries from dict.org more quickly.

;;; Code:

;; TODO: Possibly customize which thesaurus is used.

;;;###autoload
(defun jmm-dictionary-thesaurus-search (word)
  "Search WORD in moby-thesaurus."
  (interactive
   (list
    (let ((default (dictionary-search-default)))
	  (read-string (format-prompt "Thesaurus for word" default)
		       nil 'dictionary-word-history default))))
  (dictionary-new-search (cons word "moby-thesaurus")))

;;;###autoload
(defun jmm-dictionary-thesaurus-search-at-point ()
  "Search word at point in moby-thesaurus."
  (interactive)
  (dictionary-new-search (cons (current-word) "moby-thesaurus")))

(provide 'jmm-dictionary-stuff)
;;; jmm-dictionary-stuff.el ends here
