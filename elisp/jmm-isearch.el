;;; jmm-isearch.el --- Extra isearch modes and utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Joshua Moller-Mara

;; Author: Joshua Moller-Mara <jmm@cns.nyu.edu>
;; Keywords: matching

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

;; The files makes some more isearch modes that make it easy to search for a
;; group of words.

;; For example, `jmm-isearch-permuted-splits-in-line-regexp' would make an isearch for
;; "ell world" match "hello there world!"

;; Technically, we split the string by spaces, so each fragment needs
;; not be a complete word.  Also, "word" in isearch means something
;; specific, so I will call each space-separated fragment a "split" in this file.

;; Some modes this provides:
;; -  `jmm-isearch-splits-in-line-regexp'		- bound to "M-s j l"
;;     - splits in order in one line
;; -  `jmm-isearch-splits-in-symbol-regexp'		- bound to "M-s j s"
;;     - splits in order in one symbol
;; -  `jmm-isearch-splits-in-multiline-regexp'		- bound to "M-s j m"
;;     - splits in order on multiple lines
;; -  `jmm-isearch-permuted-splits-in-line-regexp'	- bound to "M-s j L"
;;     - any permutation of splits in one line
;; -  `jmm-isearch-permuted-splits-in-symbol-regexp'	- bound to "M-s j S"
;;     - any permutation of splits in one symbol
;; -  `jmm-isearch-permuted-splits-in-multiline-regexp' - bound to "M-s j M"
;;     - any permutation of splits in multiple line
  
;; The permutation modes match splits in any order, similar to the
;; "orderless" library.  I say "permuted" here to remind the user that
;; the regexp created tries every permutation, so if you have 5
;; splits, you could have 120 permutations!

;; Some of these keys are hard to press, so for more frequent usage,
;; bind keys like so:
;; (define-key isearch-mode-map (kbd "M-s l") #'isearch-toggle-permuted-splits-in-line)

;;; Code:

(defgroup jmm-isearch nil
  "JMM Utilities for isearch"
  :group 'isearch)

(defcustom jmm-isearch-highlight-submatches-extra-modes
  '(jmm-isearch-splits-in-line-regexp
    jmm-isearch-splits-in-multiline-regexp
    jmm-isearch-splits-in-symbol-regexp
    jmm-isearch-permuted-splits-in-line-regexp
    jmm-isearch-permuted-splits-in-multiline-regexp
    jmm-isearch-permuted-splits-in-symbol-regexp)
  "Extra functions that also have subexpression highlighting."
  :type '(repeat function))

(defun jmm-isearch-highlight-submatches-extra-modes-advice (orig &rest args)
  "Highlight submatches in `jmm-isearch-highlight-submatches-extra-modes'."
  (if (memq isearch-regexp-function jmm-isearch-highlight-submatches-extra-modes)
      (let ((isearch-regexp t))
	(apply orig args))
    (apply orig args)))

;; TODO: Advice should be initialized by the user, not the library.
(advice-add 'isearch-highlight :around #'jmm-isearch-highlight-submatches-extra-modes-advice)


;;;;;;;;;;
;; Internal utilities 
(defun jmm-isearch--interleave-separator (lst &rest separators)
  "Interleaves separators between list members.
(jmm-isearch-interleave-separator '(1 2 3) 'hi 'there) =>
'(1 hi there 2 hi there 3)"
  (append (mapcan (lambda (e)
		    (cons e (seq-copy separators)))
		  (butlast lst))
	  (last lst)))

(defun jmm-isearch--interleave-splits-to-regexp (string rxseparator)
  "Convert string of words to regexp, splitting words and interleaving a separator.
STRING is the input string, like \"hello world\".
RXSEPARATOR is an `rx' separator to interleave, like `(minimal-match
(0+ not-newline))' (aka \".*?\").
Returns a string regexp."
  (thread-last (split-string string nil t)
	       (seq-map-indexed (lambda (a i) `(group-n ,(1+ i) ,a)))
	       (funcall
		(lambda (l) `(sequence ,@(jmm-isearch--interleave-separator l rxseparator))))
	       rx-to-string))

(defun jmm-isearch--permutations (lst &optional testfn)
  "Return a list of all permutations of LST.
Doesn't double count equivalent permutations, which are tested using
  TESTFN.
See `seq-uniq' for TESTFN."
  (if (not lst)
      '(nil)
    (seq-uniq
     (mapcan (lambda (x)
	       (mapcar (lambda (y) (cons x y))
		       (jmm-isearch--permutations (cl-remove x lst :count 1))))
	     lst)
     testfn)))

(defun jmm-isearch--word-permutations-interleave-regexp (string rxseparator)
  "Split string into words, interleave all permutations with separator.
STRING is the input string, like \"hello world\".
RXSEPARATOR is an `rx' separator to interleave, like `(minimal-match
(0+ not-newline))' (aka \".*?\").
Returns a string regexp."
  (let ((list1 (split-string string nil t)))
    (thread-last
      (jmm-isearch--permutations
       (seq-map-indexed (lambda (a i)
			  `(group-n ,(1+ i) ,a))
			list1)
       (lambda (x y)
	 (equal (mapcar #'caddr x)
		(mapcar #'caddr y))))
      (mapcar (lambda (l) `(sequence ,@(jmm-isearch--interleave-separator l rxseparator))))
      (funcall (lambda (l) `(or ,@l)))
      (rx-to-string))))

;;;;;;;;;;
;; Modes
(defun jmm-isearch-splits-in-line-regexp (string &optional _lax)
  "Create regexp for space-separated splits in order on same line.

STRING is a space separated string of \"splits\".
For example, \"jmm-isearch regexp\" could match
\"jmm-isearch-splits-in-line-regexp\".

_LAX search is not implemented."
  ;; Easiest way:
  ;; (mapconcat 'regexp-quote (split-string string nil t) ".*")
  ;; Flexible (but slower) way:
  (jmm-isearch--interleave-splits-to-regexp string '(minimal-match (0+ not-newline))))

(isearch-define-mode-toggle splits-in-line "jl" jmm-isearch-splits-in-line-regexp)

(defun jmm-isearch-splits-in-multiline-regexp (string &optional lax)
  "Create regexp for space-separated splits in order on multiple lines.

STRING is a space separated string of \"splits\".
_LAX search is not implemented."
  (jmm-isearch--interleave-splits-to-regexp
   string
   '(sequence (minimal-match (0+ not-newline))
	      (optional "\n" (minimal-match (0+ not-newline))))))

(isearch-define-mode-toggle splits-in-multiline "jm" jmm-isearch-splits-in-multiline-regexp)

(defun jmm-isearch-splits-in-symbol-regexp (string &optional lax)
  "Create regexp for space-separated splits appearing in order in one symbol.

STRING is a space separated string of \"splits\".
For example, \"jmm-isearch regexp\" could match
\"jmm-isearch-splits-in-line-regexp\".

_LAX search is not implemented."
  (jmm-isearch--interleave-splits-to-regexp
   string
   '(minimal-match (0+ (or (syntax word) (syntax symbol))))))

(isearch-define-mode-toggle splits-in-symbol "js" jmm-isearch-splits-in-symbol-regexp)

(defun jmm-isearch-permuted-splits-in-symbol-regexp (string &optional lax)
  "Make regexp matching space-separated splits in any order in a symbol.

STRING is a space separated string of \"splits\".
For example, \"isearch regexp jmm\" could match
\"jmm-isearch-splits-in-line-regexp\".

Tries all permutations.
Beware factorial explosion!

_LAX search is not implemented."
  (jmm-isearch--word-permutations-interleave-regexp
   string
   '(minimal-match (0+ (or (syntax word) (syntax symbol))))))

(isearch-define-mode-toggle permuted-splits-in-symbol
  "jS" jmm-isearch-permuted-splits-in-symbol-regexp)

(defun jmm-isearch-permuted-splits-in-line-regexp (string &optional lax)
  "Make regexp matching space-separated splits in any order in one line.

STRING is a space separated string of \"splits\".

Tries all permutations.
Beware factorial explosion!

_LAX search is not implemented."
  (jmm-isearch--word-permutations-interleave-regexp
   string
   '(minimal-match (0+ not-newline))))

(isearch-define-mode-toggle permuted-splits-in-line
  "jL" jmm-isearch-permuted-splits-in-line-regexp)

(defun jmm-isearch-permuted-splits-in-multiline-regexp (string &optional lax)
  "Make regexp matching space-separated splits in any order in multiple lines.

Tries all permutations.
Beware factorial explosion!

_LAX search is not implemented."
  (jmm-isearch--word-permutations-interleave-regexp
   string
   '(sequence (minimal-match (0+ not-newline))
	      (optional "\n" (minimal-match (0+ not-newline))))))

(isearch-define-mode-toggle permuted-splits-in-multiline
  "jM" jmm-isearch-permuted-splits-in-multiline-regexp)


;;;;;;;;;;
;; Multi Isearch and multi occur

(defun jmm-isearch-multi-isearch-dired-buffers (&optional no-recursive-edit)
  "Search all dired buffers using `multi-isearch-buffers'."
  (interactive "P")
  (let ((buffers (cl-loop for x in (buffer-list)
			  when (provided-mode-derived-p (buffer-local-value 'major-mode x) 'dired-mode)
			  collect x))
	(isearch-mode-end-hook isearch-mode-end-hook)
	(quit nil)
	(startconfig (current-window-configuration))
	(startmarker (point-marker))
	(multi-isearch-next-buffer-function
	 'multi-isearch-next-buffer-from-list))
    (push (lambda ()
	    (when isearch-mode-end-hook-quit
	      (setq quit t)))
	  isearch-mode-end-hook)
    (setq multi-isearch-buffer-list buffers)
    (pop-to-buffer-same-window (car multi-isearch-buffer-list))
    (goto-char (point-min))
    (isearch-mode t nil nil t 'jmm-isearch-permuted-splits-in-line-regexp)
    (if quit
	(progn
	  (set-window-configuration startconfig)
	  (set-marker startmarker nil))
      (progn
	(add-to-history 'global-mark-ring startmarker global-mark-ring-max t)
	(message "Global mark saved where search started")))))

(defun jmm-isearch--get-isearch-regexp ()
  "Get a regexp for the current search.
Comes from `isearch-occur'."
  (cond
   ((functionp isearch-regexp-function)
    (funcall isearch-regexp-function isearch-string))
   (isearch-regexp-function (word-search-regexp isearch-string))
   (isearch-regexp isearch-string)
   (t (regexp-quote isearch-string))))

(defun jmm-isearch--buffers-same-mode (mode)
  "Get buffers with same major MODE."
  (cl-loop for buf in (buffer-list)
	   if (eq (buffer-local-value 'major-mode buf) mode)
	   collect buf))

(defun jmm-isearch--buffers-visiting-files ()
  "Get buffers visiting files."
  (cl-loop for buf in (buffer-list)
	   if (buffer-file-name buf)
	   collect buf))

;;;###autoload
(defun jmm-isearch-multi-occur-major-mode (regexp mode)
  "Run `multi-occur' across same major-mode buffers using search string.
Interactively, REGEXP is constructed from the isearch search string.
MODE is the major mode of the buffer.
Similar to `isearch-occur'."
  (interactive
   (list (jmm-isearch--get-isearch-regexp)
	 major-mode))
  (multi-occur (seq-reverse ;; multi-occur goes in reverse order
		(jmm-isearch--buffers-same-mode mode))
	       regexp))

;;;###autoload
(defun jmm-isearch-multi-occur-buffers (regexp &optional allbufs)
  "Run `multi-occur' across file-visiting buffers using last search string.
Interactively, REGEXP is constructed from the isearch search string.
With interactive prefix arg ALLBUFS (non-nil), search all buffers.
Similar to `isearch-occur' and `multi-occur-in-matching-buffers'."
  (interactive
   (list (jmm-isearch--get-isearch-regexp)
	 current-prefix-arg))
  (multi-occur (seq-reverse ;; multi-occur goes in reverse order
		(if allbufs (buffer-list) (jmm-isearch--buffers-visiting-files)))
	       regexp))

;;;###autoload
(defun jmm-isearch-multi-occur-major-mode-permuted-splits-in-line (string mode)
  "Run `multi-occur' across same major MODE buffers using permuted search STRING.
STRING is transformed using `jmm-isearch-permuted-splits-in-line-regexp'.
MODE is the major mode of the buffer."
  (interactive
   (list (read-string "Permuted major-mode multi occur: " nil 'search-ring)
	 major-mode))
  (multi-occur (seq-reverse ;; multi-occur goes in reverse order
		(jmm-isearch--buffers-same-mode mode))
	       (jmm-isearch-permuted-splits-in-line-regexp string)))

;;;###autoload
(defun jmm-isearch-multi-occur-permuted-splits-in-line (string &optional allbufs)
  "Run `multi-occur' across file-visiting buffers using permuted search STRING.
If interactive prefix arg ALLBUFS is non-nill, search all buffers.
STRING is transformed using `jmm-isearch-permuted-splits-in-line-regexp'."
  (interactive
   (list (read-string "Permuted multi occur: " nil 'search-ring)
	 current-prefix-arg))
  (multi-occur (seq-reverse ;; multi-occur goes in reverse order
		(if allbufs (buffer-list) (jmm-isearch--buffers-visiting-files)))
	       (jmm-isearch-permuted-splits-in-line-regexp string)))

(define-key isearch-mode-map (kbd "M-s j o") #'jmm-isearch-multi-occur-major-mode)
(define-key isearch-mode-map (kbd "M-s j b") #'jmm-isearch-multi-occur-buffers)


;;;;;;;;;;
;; Delete region

(defun jmm-isearch-delete-region (&optional nomove)
  "End the current isearch, deleting the region from starting end.
If interactive prefix arg NOMOVE is non-nil, don't move to the starting end.
Basically, you can use this like a more complicated `zap-to-char' that doesn't kill."
  (interactive)
  (isearch-done)
  (let ((pos (if nomove
		 (point)
	       (if isearch-forward
		   isearch-other-end
		 (point)))))
    (goto-char pos)
    (delete-region (mark t) (point))))

(define-key isearch-mode-map (kbd "C-<backspace>") #'jmm-isearch-delete-region)

(provide 'jmm-isearch)
;;; jmm-isearch.el ends here
