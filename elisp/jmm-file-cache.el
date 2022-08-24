;;; jmm-file-cache.el --- Utilities for adding things to file cache  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Joshua Moller-Mara

;; Author: Joshua Moller-Mara <jmm@cns.nyu.edu>
;; Keywords: convenience, files

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

;; This replicates (or "rips" rather) a lot of filecache code, but
;; changes when things error.
;;
;; Some of these functions might be better rewritten as advice for the
;; normal filecache functions.
;; 
;; Once you've added files with `jfc--add-file', you'll want to bind
;; `jfc-complete-file-cache' to some key in the minibuffer.
;; For example, 
;; (define-key minibuffer-local-filename-completion-map (kbd "M-i") 'jfc-complete-file-cache)

;; TODO: Automatically "track" new projects, so we can rescan them later.

;; TODO: Allow completing directories of projects as well as files

;; MAYBE: Use "multisession" variables to hang onto the file cache
;; instead of rebuilding it each time.

;;; Code:
(require 'filecache)

(defvar jfc-file-attribute-alist nil
  "An alist of cached files and their associated metadata")

(defvar jfc-filename-newest-alist nil
  "An alist of cached files and the modification time of the newest file")

;; TODO: Make interactive again
(defun jfc--add-file (file)
  "Like `file-cache-add-file', but less strict.
Doesn't error out if the file doesn't exist.
Adds some metadata for sorting later."
  (setq file (expand-file-name file))
  (when (file-exists-p file)
    (let* ((file-name (if (file-directory-p file)
			  (concat (file-name-nondirectory file) "/")
			(file-name-nondirectory file)))
	   (dir-name  (file-name-directory file))
	   (attributes (file-attributes file))
	   (modtime (file-attribute-modification-time attributes))
	   (lastfiletime (alist-get file-name jfc-filename-newest-alist nil nil #'equal))
	   (the-entry (assoc-string file-name file-cache-alist
				    file-cache-ignore-case)))
      (setf (alist-get file jfc-file-attribute-alist nil nil #'equal)
	    attributes)
      ;; TODO: Maybe store the path of the newest file instead of duplicating the file modification time?
      ;; MAYBE: Store the last time we opened something, just like `recent-track-opened-file'?
      (when (or (null lastfiletime) (time-less-p lastfiletime modtime))
	(setf (alist-get file-name jfc-filename-newest-alist nil nil #'equal)
	      modtime))
      (cond ((null the-entry)
	     ;; If the entry wasn't in the cache, add it.
	     (push (list file-name dir-name) file-cache-alist))
	    ((not (member dir-name (cdr the-entry)))
	     ;; Add directory to cache
	     (setcdr the-entry (cons dir-name (cdr the-entry))))))))

(defun jfc-track-opened-file ()
  "Kind of like `recentf-track-opened-file', but adds to file cache.
Only really used when writing files."
  (and buffer-file-name
       ;; Only add if we've already added it before
       (alist-get (expand-file-name buffer-file-name) jfc-file-attribute-alist nil nil #'equal)
       (jfc--add-file buffer-file-name))
  ;; Must return nil because it is run from `write-file-functions'.
  nil)

(defun jfc--add-from-buffer ()
  "See `file-cache--add-from-buffer'.
Does the same thing, but doesn't error out for non-existing files.
Uses `jfc--add-file' instead."
  (dolist (elt file-cache-filter-regexps)
    (goto-char (point-min))
    (delete-matching-lines elt))
  (goto-char (point-min))
  (while (re-search-forward file-cache-buffer-default-regexp nil t)
    (jfc--add-file (match-string-no-properties 0))))

;; TODO: Make interactive?
;; TODO: Maybe add subdirectories?
(defun jfc-add-files-from-project (dir)
  "Add files in a project DIR."
  (jfc--add-file dir)
  (let ((myfiles (project-files (project-current nil dir))))
    (with-temp-buffer
      (dolist (file myfiles)
	(insert file)
	(insert "\n"))
      (jfc--add-from-buffer))))

;; TODO: Make interactive?
(defun jfc-add-files-from-projects (dirs)
  "Add all files in a list of DIRS of projects."
  (dolist (dir dirs)
    (jfc--add-file dir)
    (jfc-add-files-from-project dir)))

(defun jfc-add-file-list (files)
  "Add all files in a list of FILES."
  (dolist (file files)
    (jfc--add-file file)))

(defun jfc-add-files-from-directories (dirs)
  "Add all files in a list of DIRS."
  (cl-labels ((is-filtered-p (file)
		(seq-some (lambda (regexp) (string-match-p regexp file))
			  file-cache-filter-regexps))
	      (filter-files (files)
		(seq-remove #'is-filtered-p files)))
      (dolist (dir dirs)
	(let* ((dir (expand-file-name dir))
	       (dir-files (directory-files dir t nil)))
	  (jfc--add-file dir)
	  (jfc-add-file-list (filter-files dir-files))))))

(defun jfc--directory-completions (matches)
  (let* ((collection matches)
	 (sorting (lambda (strings)
		    ;; TODO: Sort by file time?
		    (seq-sort-by (lambda (e)
				   (or (seq-position collection e) most-positive-fixnum))
				 #'< strings))))
    (lambda (string pred action)
      (if (eq action 'metadata)
	  `(metadata (category . jfc-directories)
		     (display-sort-function . ,sorting))
	(complete-with-action action collection string pred)))))

(defun jfc--file-completions ()
  (let* ((collection file-cache-alist)
	 (sorting (lambda (strings)
		    (seq-sort-by (lambda (e)
				   (alist-get e jfc-filename-newest-alist nil nil #'equal))
				 (lambda (x y)
				   (time-less-p y x))
				 strings))))
    (lambda (string pred action)
      (if (eq action 'metadata)
	  `(metadata (category . jfc-files)
		     (display-sort-function . ,sorting))
	(complete-with-action action collection string pred)))))

(add-to-list 'completion-category-defaults
	     '(jfc-directories (styles substring)))

(add-to-list 'completion-category-defaults
	     '(jfc-files (styles basic substring)))

(defun jfc-complete-file-cache ()
  "Tries to read a file from the file cache.
You probably need recursive minibuffers enabled for this to work."
  (interactive)
  (let* ((collection (jfc--file-completions))
	 (string (file-name-nondirectory (minibuffer-contents)))
	 (completion1 (completion-try-completion string collection nil (length string)))
	 (completion1 (cond
		       ((eq completion1 t) string)
		       (t (car completion1))))
	 (completion2 (when completion1
			  (completion-try-completion completion1 collection nil (length completion1))))
	 (file1 (cond
		 ((eq completion2 t) completion1)
		 (t (completing-read "File: "  collection nil nil string))))
	 (matches (alist-get file1 file-cache-alist nil nil #'equal))
	 (match (if (length= matches 1)
		    (car matches)
		  (completing-read (format "Directory for \"%s\": " file1) (jfc--directory-completions matches)))))
    (completion--replace (minibuffer--completion-prompt-end) (point-max) (concat match file1))))

;; These basically copy from `recentf-mode'
(defconst jfc-recentf-used-hooks
  '(;; (find-file-hook       jfc-track-opened-file)
    ;; Only update if we save a buffer, which can change the modification time.
    (write-file-functions jfc-track-opened-file))
  "Hooks used by jfc-recentf-mode.")

(defvar-keymap jfc-recentf-mode-map
  :doc "Keymap to use in `jfc-recentf-mode'.")

(define-minor-mode jfc-recentf-mode
  "Tracks saved files and updates the file cache.
This is mostly used so we can sort by most recently used files."
  :global t
  :keymap jfc-recentf-mode-map
  (let ((hook-setup (if jfc-recentf-mode 'add-hook 'remove-hook)))
    (dolist (hook jfc-recentf-used-hooks)
      (apply hook-setup hook))))

(provide 'jmm-file-cache)
;;; jmm-file-cache.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("jfc-" . "jmm-file-cache-"))
;; End:
