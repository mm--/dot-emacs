;;; minibuffer-jmm.el --- Utilities for the minibuffer  -*- lexical-binding: t; -*-

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

;; These are some commands that make minibuffer completion easier.

;; TODO: Make a function that cycles between the most recently modified files.
;;  It should use the minibuffer to find the newest matching file.
;; TODO: Complete file name by searching the current directory recursively. Make it easy to quickly drill down or find files.

;;; Code:

;;;###autoload
(defun jmm/minibuffer-complete-cycle ()
  "Run `minibuffer-complete', but set `completetion-cycle-threshold' to t so we always cycle."
  (interactive)
  (let ((completion-cycle-threshold t))
    (minibuffer-complete)))

;;;###autoload
(defun jmm/file-minibuffer-up-dir (arg)
  "Go up ARG directories in the minibuffer."
  (interactive "P")
  ;; TODO: Repeat arg times.
  (let* ((thefile (minibuffer-contents))
	 (thefileexpanded (expand-file-name thefile))
	 (is-expanded (string= thefile thefileexpanded))
	 (thedir (file-name-directory
		  (if (directory-name-p thefileexpanded)
		      (directory-file-name thefileexpanded)
		    thefile))))
    (delete-minibuffer-contents)
    (insert (if is-expanded
		thedir
	      (abbreviate-file-name thedir)))))

;;;###autoload
(defun jmm/file-minibuffer-project-root-dir ()
  "Go up to the project root."
  (interactive)
  (let* ((thefile (minibuffer-contents))
	 (thefileexpanded (expand-file-name thefile))
	 (is-expanded (string= thefile thefileexpanded))
	 (pr (project-current nil thefile)))
    (if pr
	(progn
	  (delete-minibuffer-contents)
	  (insert (if is-expanded
		      (expand-file-name (project-root pr))
		    (project-root pr))))
      (minibuffer-message "No project root"))))

(provide 'minibuffer-jmm)
;;; minibuffer-jmm.el ends here
