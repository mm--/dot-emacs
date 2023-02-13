;;; hippie-expand-jmm.el --- Extra hippie-expand functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Joshua Moller-Mara

;; Author: Josh Mōller-Mara <jmm@cns.nyu.edu>
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

;; This contains some extra completion functions for hippie-expand.
;; Names are subject to change

;; - `try-expand-multiabbrevs' - Allows you to define multiple ways to expand one abbreviation.
;; - `try-expand-local-abbrevs'
;; - `try-expand-local-abbrevs2' - Allows expanding skeletons
;; - `try-expand-global-abbrevs'
;; - `try-expand-acronym' - "tea." -> "try-expand-acronym"
;; - `try-expand-acronym-all-buffers'
;; - `try-expand-flex-dabbrev' - "texpv" -> "try-expand-flex-dabbrev"
;; - `try-expand-flex-dabbrev-all-buffers'
;; - `try-expand-dabbrev2' - Tries to mimic dabbrev-expand more closely
;; - `try-minibuffer-expand-bookmark' - Expand bookmarks in find-file
;; - `try-minibuffer-expand-buffer' - Flex search recent buffers in switch-to-buffer
;; - `try-complete-project-file-name' - Makes it easier to complete relative filenames

;; There's a function that allows "flex" expansion.
;; For example, "rpty" -> "reproducibility"
;; The first and last letters must match, and all internal letters
;; must be in order.

;; There's a function that allows for acronym expansion.
;; For example, "hss." -> "he-search-string"
;; Note that it has to end with a period to signal an acronym.

;; A couple functions are modified versions of code from
;; hippie-exp.el, and the author for that is Anders Holst <aho@sans.kth.se>

;;; Code:
(require 'dash)
(require 'bookmark) ;; For `try-minibuffer-expand-bookmark'


;;; Internal functions
;; You specify trans-case by setting substitute-function to (lambda (x) (he-substitute-string x t))
(defun he--all-buffers2 (old init-function search-function substitute-function)
  "Just like `he--all-buffers', but initializes differently.
Instead of passing a function that goes to the point, you have to initialize the beginning, end, and `he-search-string' yourself."
  (let ((expansion ())
        (buf (current-buffer))
        (only-buffers hippie-expand-only-buffers)
        (ignore-buffers hippie-expand-ignore-buffers)
        (orig-case-fold-search case-fold-search))
    (if (not old)
        (progn
	  (funcall init-function)
	  (setq he-search-bufs (buffer-list))
	  (setq he-searched-n-bufs 0)
	  (set-marker he-search-loc 1 (car he-search-bufs))))

    (if (not (equal he-search-string ""))
	(while (and he-search-bufs
		    (not expansion)
		    (or (not hippie-expand-max-buffers)
                        (< he-searched-n-bufs hippie-expand-max-buffers)))
          (set-buffer (car he-search-bufs))
          (if (and (not (eq (current-buffer) buf))
                   (if only-buffers
                       (he-buffer-member only-buffers)
                     (not (he-buffer-member ignore-buffers))))
              (save-excursion
                (save-restriction
                  (if hippie-expand-no-restriction
		      (widen))
		  (goto-char he-search-loc)
		  (setq expansion
			(let ((case-fold-search orig-case-fold-search))
			  (funcall search-function he-search-string)))
		  (set-marker he-search-loc (point))
		  (if (not expansion)
		      (progn
			(setq he-search-bufs (cdr he-search-bufs))
			(setq he-searched-n-bufs (1+ he-searched-n-bufs))
			(set-marker he-search-loc 1 (car he-search-bufs))))))
	    (setq he-search-bufs (cdr he-search-bufs))
	    (set-marker he-search-loc 1 (car he-search-bufs)))))

    (set-buffer buf)
    (if (not expansion)
	(progn
	  (if old (he-reset-string))
	  ())
      (progn
        ;; (he-substitute-string expansion t)
	(funcall substitute-function expansion)
        t))))


;; DONE: Specify trans-case option explicitly
;; Or maybe allow us to supply our own he-substitute-string.
;; You supply your own substitute-function that takes one argument: the expansion.
;; So, if you want to trans-case, you need to pass (lambda (x) (he-substitute-string x t))
(defun he--one-buffer (old init-function search-function substitute-function)
  "Like the stuff in `try-expand-line', but genericized and allows for a different init-function."
  (let ((expansion ()))
    (if (not old)
	(progn
	  ;; I'm putting this before the init-function in case we want to override it.
	  (setq he-search-bw t)
	  ;; Maybe I should allow the init-function to override he-search-loc
	  ;; Like, I set it to be empty, and if it touches it then I don't.
	  (funcall init-function)
	  (set-marker he-search-loc he-string-beg)))

    (if (not (equal he-search-string ""))
	(save-excursion
	  (save-restriction
	    (if hippie-expand-no-restriction
		(widen))
	    ;; Try looking backward unless inhibited.
	    (if he-search-bw
		(progn
		  (goto-char he-search-loc)
		  (setq expansion (funcall search-function he-search-string t))
		  (set-marker he-search-loc (point))
		  (if (not expansion)
		      (progn
			(set-marker he-search-loc he-string-end)
			(setq he-search-bw ())))))

	    (if (not expansion) ; Then look forward.
		(progn
		  (goto-char he-search-loc)
		  (setq expansion (funcall search-function he-search-string nil))
		  (set-marker he-search-loc (point)))))))

    (if (not expansion)
	(progn
	  (if old (he-reset-string))
	  ())
      (progn
	;; (he-substitute-string expansion t)
	(funcall substitute-function expansion)
	t))))


(defun jmm-hippie--remove-tried (candidates)
  "Remove CANDIDATES that have already been tried.
CANDIDATES is a list of strings."
  (seq-remove (lambda (x) (he-string-member x he-tried-table t)) candidates))

(defun jmm-hippie--remove-low-effort (candidates)
  "Remove CANDIDATES that only expand fewer than 2 characters."
  (let* ((baselen (length he-search-string)))
    (seq-remove (lambda (word) (length< word (+ baselen 2))) candidates)))


;;; "Multiabbrev" expansion

;; If this needed to be expanded more, it should probably be a hashtable
(defvar jmm/multiabbrevs-alist '( ;; Kaomojis, from http://kaomoji.ru/en/
				 ("kjoy" "(* ^ ω ^)" "(´ ∀ ` *)" "٩(◕‿◕｡)۶" "☆*:.｡.o(≧▽≦)o.｡.:*☆")
				 ("kanger" "٩(╬ʘ益ʘ╬)۶" "(ﾉಥ益ಥ)ﾉ" "┌∩┐(◣_◢)┌∩┐" "(҂` ﾛ ´)凸" "(╯°益°)╯彡┻━┻")
				 ("ksad" "(T_T)" "(Ｔ▽Ｔ)")
				 ("kwhy" "ლ(ಠ_ಠ ლ)" "ლ(¯ロ¯\"ლ)")
				 ("krun" "ε=ε=ε=ε=┌(;￣▽￣)┘" "ε=ε=┌( >_<)┘")
				 (("surprise" "ksp") "(⊙_⊙)")
				 (("shrug" "kshrug") "¯\\_(ツ)_/¯" "┐(シ)┌" "ᕕ( ᐛ )ᕗ")
				 ("shrug2" "¯\\\\_(ツ)_/¯" )
				 ("kface" "( ͡° ͜ʖ ͡°)" "( ͡° ʖ̯ ͡°)" "( ͠° ͟ʖ ͡°)")
				 (("kcool" "ksunglasses") "(⌐■_■)")
				 (("tableflip" "tbfl" "ktbl") "(╮°-°)╮┳━━┳" "(╯°□°)╯︵ ┻━━┻" "┬─┬ノ( º _ ºノ)" "(╯°益°)╯彡┻━┻" "┻━┻︵ヽ(`Д´)ﾉ︵ ┻━┻")
				 ("kbear" "ʕ •ᴥ•ʔ")
				 ("jmm" "Josh Moller-Mara" "Josh Mōller-Mara")
				 ;; Squintmojis
				 (("squintmoji" "khsr") "　🚘\n　⛔️🏓\n🥊🍘\n　🎵" "🛁\n🚨\n🍶" "　🛁\n　🖇🚨　⛴\n⛴　　✔️" "　🔍🦎\n🥒🔋\n 🦎🐢" "🎷👂🏻\n　👛")
				 )
  "This should be an alist of abbrevs and their expansions. This first element can be a list of synonyms to expand.")

;; TODO: Maybe remember the last one? Though that could be annoying and unpredictable.
;;;###autoload
(defun try-expand-multiabbrevs (old)
  "Try to expand word before point according to `jmm/multiabbrevs-alist'"
  (if (not old)
      (progn
	(he-init-string (he-dabbrev-beg) (point))
	(setq he-expand-list
	      (and (not (equal he-search-string ""))
		   (cdr (assoc he-search-string jmm/multiabbrevs-alist
			       ;; The first element of the alist can
			       ;; be a list if there are synonyms
			       (lambda (list elt)
				 (if (listp list)
				     (member elt list)
				   (equal elt list)))))))))

  (let (nextstr)
    (while (and he-expand-list
		(not nextstr))
      (let ((nextelt (pop he-expand-list)))
	(cond
	 ((null nextelt) nil) ;; Just ignore nil values
	 ((stringp nextelt)
	  (if (he-string-member nextelt he-tried-table t)
	      nil ;; Do nothing with it, since we tried it already.
	    (progn
	      (setq nextstr nextelt)
	      (push nextstr he-expand-list)))) ;; This is the only place we should break.
	 ((listp nextelt) (dolist (elt (reverse nextelt)) (push elt he-expand-list)))
	 ((and (symbolp nextelt)
	       (boundp nextelt))
	  (when-let ((symval (symbol-value nextelt)))
	    (push symval he-expand-list)))
	 ((and (symbolp nextelt)
	       (symbol-function nextelt))
	  (push (funcall (symbol-function nextelt)) he-expand-list))
	 ))))

  ;; This should be skipped, if everything's correct above.
  ;; (while (and he-expand-list
  ;; 	      (or (not (car he-expand-list))
  ;; 		  (he-string-member (car he-expand-list) he-tried-table t)))
  ;;   (setq he-expand-list (cdr he-expand-list)))

  (if (null he-expand-list)
      (progn
	(if old (he-reset-string))
	())
      (progn
	(he-substitute-string (car he-expand-list))
	(setq he-expand-list (cdr he-expand-list))
	t)))


;;; Modified "abbrev" expansion.
;; Allows expansion of skeletons

(defvar jmm/he-last-expanded-function nil)

;; FIXME: No way to undo this, for now.
;; Check out atomic change groups `prepare-change-group'.
;; - Oh! Skeletons use `atomic-change-group'! Nice.
;; FIXME: Intercept quitting. Reset if not.
(defun jmm/he-substitute-function (funsym)
    (goto-char he-string-beg)
    (setq he-tried-table (cons (concat "jmm-command " (symbol-name funsym)) he-tried-table))
    (delete-region (point) he-string-end)
    (funcall funsym))

(defun jmm/he-substitute-function2 (funsym)
  "Replace string with function call."
  (undo-boundary)
  (atomic-change-group
    (goto-char he-string-beg)
    (setq he-tried-table (cons (concat "jmm-abbrev " (symbol-name funsym)) he-tried-table))
    (delete-region (point) he-string-end)
    (funcall funsym)))

;; TODO: Split out expanding of normal text. This should probably only expand functions.
;; TODO: Mark this function with some kind if "impure" metadata, so other completions can avoid this.
;; TODO: Detect if the skeleton aborted with a quit. If so, return nil.
;;;###autoload
(defun try-expand-local-abbrevs2 (old)
  "Like `try-expand-local-abbrevs', but can expand skeletons.
The abbrev needs to be associated with the symbol 'jmm-command."
  (if (not old)
      (progn
	(setq jmm/he-last-expanded-function nil)
	(he-init-string (he-dabbrev-beg) (point))
	(let* ((he-down-str (downcase he-search-string))
	       (expansion (abbrev-expansion he-down-str local-abbrev-table)))
	  (cond
	   ((stringp expansion)
	    (unless (he-string-member expansion he-tried-table t)
	      (progn (he-substitute-string expansion) t)))
	   ((eq expansion 'jmm-command)
	    ;; We're expanding some skeleton or command.
	    (let ((curcommand this-command)
		  (symfun (symbol-function (abbrev-symbol he-down-str local-abbrev-table))))
	      ;; Maybe amalgamating-undo
	      (jmm/he-substitute-function symfun)
	      ;; Somehow this-command gets obliterated if you prompt.
	      (setq this-command curcommand)
	      (setq jmm/he-last-expanded-function t)
	      t)))))
    (progn
      (if jmm/he-last-expanded-function
	  (undo)
	(he-reset-string))
      nil)))

;;;###autoload
(defun try-expand-global-abbrev-hooks (old)
  "Like `try-expand-global-abbrevs', but only expands hooks.
Basically, the abbrev needs to be an empty string and have a hook."
  (if (not old)
      (progn
	(he-init-string (he-dabbrev-beg) (point))
	(let* ((expansion (abbrev-expansion he-search-string global-abbrev-table))
	       (curcommand this-command)
	       sym symfun)
	  (when (and (stringp expansion) (string-empty-p expansion)
		     (setq sym (abbrev-symbol he-search-string global-abbrev-table))
		     (setq symfun (symbol-function sym)))
	    ;; Maybe amalgamating-undo
	    (jmm/he-substitute-function2 symfun)
	    ;; Somehow this-command gets obliterated if you prompt.
	    (setq this-command curcommand)
	    t)))
    (progn
      (undo)
      nil)))

(defun try-expand-local-abbrevs3 (old)
  "Like `try-expand-local-abbrevs2', but can expand skeletons.
Unlike `try-expand-local-abbrevs2', doesn't require 'jmm-command to expand skeletons.
But the expansion should be blank."
  (if (not old)
      (progn
	(setq jmm/he-last-expanded-function nil)
	(he-init-string (he-dabbrev-beg) (point))
	(let* ((he-down-str (downcase he-search-string))
	       (expansion (abbrev-expansion he-down-str local-abbrev-table))
	       symfun)
	  (cond
	   ((and (stringp expansion)
		 (not (string-empty-p expansion)))
	    (unless (he-string-member expansion he-tried-table t)
	      (progn (he-substitute-string expansion) t)))
	   ((and (string-empty-p expansion)
		 (setq symfun (symbol-function (abbrev-symbol he-down-str local-abbrev-table))))
	    ;; We're expanding some skeleton or command.
	    ;; NOTE: Won't be able to undo a non-skeleton yet.
	    (let ((curcommand this-command))
	      ;; Maybe amalgamating-undo
	      (jmm/he-substitute-function symfun)
	      ;; Somehow this-command gets obliterated if you prompt.
	      (setq this-command curcommand)
	      (setq jmm/he-last-expanded-function t)
	      t)))))
    (progn
      (if jmm/he-last-expanded-function
	  (undo)
	(he-reset-string))
      nil)))

;;;###autoload
(defun try-expand-local-abbrevs (old)
  "Like `try-expand-local-abbrevs2', but simpler and doesn't expand skeletons."
  (if (not old)
      (progn
	(he-init-string (he-dabbrev-beg) (point))
	(let* ((he-down-str (downcase he-search-string))
	       (expansion (abbrev-expansion he-down-str local-abbrev-table)))
	  (when (and (stringp expansion)
		     (not (he-string-member expansion he-tried-table t)))
	    (he-substitute-string expansion t)
	    t)))
    (progn
      (he-reset-string)
      nil)))

;;;###autoload
(defun try-expand-global-abbrevs (old)
  "Expand only global abbrevs."
  (if (not old)
      (progn
	(he-init-string (he-dabbrev-beg) (point))
	(let* ((he-down-str (downcase he-search-string))
	       (expansion (abbrev-expansion he-down-str global-abbrev-table)))
	  (when (and (stringp expansion)
		     (not (string-empty-p expansion)) ;; Don't expand an empty thing.
		     (not (he-string-member expansion he-tried-table t)))
	    (he-substitute-string expansion t)
	    t)))
    (progn
      (he-reset-string)
      nil)))



;;; Acronym expansion

(defun he-acronym-init ()
  (if (looking-back "\\." (line-beginning-position))
    (let ((op (point))
	  beg end)
      (save-excursion
	(set-marker he-string-end (point))
	(skip-chars-backward ".")
	(setq end (point))
	(skip-syntax-backward "w")
	(setq beg (point))
	(set-marker he-string-beg beg)
	(setq he-search-string (buffer-substring-no-properties beg end))))
    (setq he-search-string "")))

(defun he-acronym-search (pattern &optional reverse limit)
  "Like `he-dabbrev-search', but searches for acronyms."
  (let ((result ())
	(case-fold-search nil)
	(regpat (apply #'concat
		       (-interpose
			;; FIXME: Other types of hyphens or word separators?
			"\\(\\s *\\|[-/]\\|\n\\s *\\)"
			(mapcar (lambda (char)
				  (if (string-match-p "[[:upper:]]" char)
				      (format "\\b%s[[:alpha:]]*\\b" char)
				      (format "\\b[%s%s][[:alpha:]]*\\b"
					      (regexp-quote (downcase char))
					      (regexp-quote (upcase char)))))
				(string-glyph-split pattern))))))
    (while (and (not result)
		(if reverse
		    (re-search-backward regpat limit t)
		  (re-search-forward regpat limit t)))
      ;; Replace line breaks here instead of at expansion, that way we can check if it's tried.
      (setq result (replace-regexp-in-string "\n\\s *" " "
					     (buffer-substring-no-properties (match-beginning 0)
									     (match-end 0))))
      (if (or (he-string-member result he-tried-table t) ;; ignore if already in table
	      (equal result pattern)) ;; or if it's the same as the pattern
	  ;; (We could only allow flex expansions that add some number of characters)
	  (setq result nil)))
    result))

;; TODO: Implement capitalization.

;; TODO: Implement continuing from last point, if we're filling a space.
;; Need to provide a marker and a way of moving forward.
;; Maybe should provide a test-function. Like, tests if we're looking at a space. Then inits?
;;;###autoload
(defun try-expand-acronym-all-buffers (old)
  "It's `try-expand-acronym', but searches all buffers."
  (he--all-buffers2 old #'he-acronym-init #'he-acronym-search #'he-substitute-string))

;;;###autoload
(defun try-expand-acronym (old)
    "Tries to expand an acronym like \"atm.\". Strips out period and looks backwards.
Needs to have that period at the end."
  (he--one-buffer old #'he-acronym-init #'he-acronym-search #'he-substitute-string))


;;; Flex expansion

(defvar he-flex-memoized-patterns '())


(defun he-flex-beg ()
  (let ((op (point)))
    (save-excursion
      (if (= (skip-chars-backward "a-zA-Z.") 0)
	  op
	(point)))))

;; DONE: Flex should only work for alpha characters. It should refuse to expand things like "tg."
(defun he-flex-init ()
  (he-init-string (he-flex-beg) (point))
  (if (string-match-p "^[-a-z.]+[a-z]$" he-search-string)
      (setq he-flex-memoized-patterns '())
    (setq he-search-string "")))

;; Maybe the pattern should be created outside?
;; Not sure how often this runs, or if it matters.
;; TODO: Would be cool to have this do something different in the minibuffer. Like, only search symbols.
;; DONE: Allow multiword flex search by using periods.
;; TODO: See if this pattern creation is slow. Maybe memoize it.
(defun he-flex-search (pattern &optional reverse limit)
  "Like `he-dabbrev-search', but with a flex pattern.
Allows you to expand lisp symbols that contain a hyphen or slash."
  (let* ((result ())
	 (case-fold-search nil)
	 (separator (if (eq major-mode 'emacs-lisp-mode)
			"[-/a-zA-Z]*" ;; Only allow slashes from Emacs Lisp.
		      "[-a-zA-Z]*"))
	 ;; TODO: This memoization is wrong, since it doesn't change for lisp mode.
	 ;; TODO: This memoization might be unnecessary 
	 (regpat (with-memoization (alist-get pattern he-flex-memoized-patterns nil nil #'equal)
		   (let* ((glyphlist (string-glyph-split pattern))
			  (res '())
			  lastchar first hasspace)
		     (while glyphlist
		       (let ((char (car glyphlist)))
			 (when (not first)
			   (push "\\b" res)
			   (setq first t))

			 (if (string-equal char ".")
			     (progn
			       (setq hasspace t)
			       (when (string-equal lastchar ".")
				 (push separator res))
			       (push "\\([[:space:]]\\|\n\\)+" res))
			   (progn
			     (when (string-equal lastchar ".")
			       (push "\\b" res))
			     (if (string-match-p "[[:upper:]]" char)
				 (push (regexp-quote char) res)
				 (push (format "[%s%s]"
					      (regexp-quote (downcase char))
					      (regexp-quote (upcase char)))
				       res))
			     (push separator res)))
			 (setq lastchar char)
			 (setq glyphlist (cdr glyphlist))))
		     (unless hasspace
		       (push "\\b" res))
		     (apply #'concat (reverse res))))))
    (while (and (not result)
		(if reverse
		    (re-search-backward regpat limit t)
		  (re-search-forward regpat limit t)))
      (setq result (buffer-substring-no-properties (match-beginning 0)
						   (match-end 0)))
      (if (or (he-string-member result he-tried-table t) ;; ignore if already in table
	      (equal result pattern)) ;; or if it's the same as the pattern
	  ;; (We could only allow flex expansions that add some number of characters)
	  (setq result nil)))
    result))

(defun he-substitute-string-transcase (expansion)
  (he-substitute-string expansion t))

;;;###autoload
(defun try-expand-flex-dabbrev-all-buffers (old)
  "It's `try-expand-flex-dabbrev', with all buffers."
  (he--all-buffers2 old #'he-flex-init #'he-flex-search #'he-substitute-string))

;;;###autoload
(defun try-expand-flex-dabbrev (old)
    "A flexible type of dynamic abbreviation.
For example \"rblty\" could expand to \"reproducibility\".
Kind of like `try-expand-dabbrev'."
  (he--one-buffer old #'he-flex-init #'he-flex-search #'he-substitute-string))


;;; "dabbrev2"
;; Like "try-expand-dabbrev", but allows you to expand subsequent words with spaces.


(defvar he-dabbrev2--lastused nil
  "Was `try-expand-dabbrev-historical' the last one used?")
(defvar he-dabbrev2--last-location (make-marker))

(defvar he-dabbrev2--last-expansion (make-marker))

;; TODO: If there's no previous expansion, search for the previous
;; word (I guess somewhere), then continue from there.


(defun he-dabbrev2-forward-loc ()
  (let (expansion)
    (save-excursion
      (save-restriction
	(if hippie-expand-no-restriction
	    (widen))
	(goto-char he-dabbrev2--last-location)
	;; Go forward over last place and spaces
	;; FIXME: There could just be a line break without spaces
	(re-search-forward "\\<\\(\\sw\\|\\s_\\)+\\([[:space:]]\\|\n\\)+" nil t)
	(set-marker he-dabbrev2--last-location (point))
	;; The next one is the symbol we want to save.
	(re-search-forward "\\<\\(\\sw\\|\\s_\\)+" nil t)
	(setq expansion (buffer-substring-no-properties (match-beginning 0)
							(match-end 0)))
	(if (he-string-member expansion he-tried-table t) ; ignore if already in table
	    (setq expansion nil))))
    expansion))

;;;###autoload
(defun try-expand-dabbrev2 (old)
  "Like `try-expand-dabbrev', but expands spaces using the words following the dabbrev."
  (let (expansion)
    (if (and (not old)
	     (= (preceding-char) ?\s)
	     (markerp he-dabbrev2--last-expansion)
	     (= (point) (1+ he-dabbrev2--last-expansion)))
	(progn
	  ;; Looks like he-init-string needs to not be empty for
	  ;; he-substitute-string to work
	  (he-init-string (1- (point)) (point))
	  (setq expansion (he-dabbrev2-forward-loc))
	  (when expansion
	    (he-substitute-string (concat " " expansion) t)
	    (set-marker he-dabbrev2--last-expansion (point))
	    t))
      (if (try-expand-dabbrev old)
	  (progn
	    (set-marker he-dabbrev2--last-location he-search-loc)
	    (set-marker he-dabbrev2--last-expansion (point))
	    t)
	(progn
	  (set-marker he-search-loc nil)
	  (set-marker he-dabbrev2--last-expansion nil)
	  nil)
	))))

(defvar jmm-hippie--try-expand-window-dabbrev-last-loc nil
  "Last place we found an expansion for `jmm-try-expand-window-dabbrev'.")
(defvar jmm-hippie--try-expand-window-dabbrev-last-expansion nil
  "Last place we expanded `jmm-try-expand-window-dabbrev'.")

(defun jmm-try-expand-window-dabbrev (old)
  "Try to expand a dynamic abbreviation using only the current window."
  (if (and (not old) ;; If we're continuing the last expansion
	   (= (preceding-char) \s)
	   (markerp jmm-hippie--try-expand-window-dabbrev-last-expansion)
	   (= (point) (1+ jmm-hippie--try-expand-window-dabbrev-last-expansion)))))


;;; Minibuffer expansion

;; DONE: Maybe make it search bookmarks with files.
;; MAYBE: Require it ends in a comma to look for a bookmark.
;; Or like ",b" for bookmarks. ",p" for projects ",d" for open dired buffers.
;;;###autoload
(defun try-minibuffer-expand-bookmark (old)
  "Expand a bookmark in the minibuffer, only if we're in the minibuffer, and if it has a filename."
  (if (not old)
      ;; Are we looking for a file?
      ;; MAYBE: We could also check the completion table metadata 'category.
      (when (eq minibuffer-completion-table 'read-file-name-internal)
	(bookmark-maybe-load-default-file)
	;; This way doesn't work
	;; (memq minibuffer-local-filename-completion-map
	;; 	    (current-active-maps))
	(he-init-string (minibuffer-prompt-end) (point-max))
	(let* ((bkname (file-name-nondirectory (minibuffer-contents)))
	       (bk (bookmark-get-bookmark bkname t))
	       bkfile)
	  (when (and bk (setq bkfile (bookmark-get-filename bk)))
	    (he-substitute-string bkfile)
	    t)))
    (progn
      (he-reset-string)
      nil)))

;;;###autoload
(defun try-minibuffer-expand-buffer (old)
  "Expand a buffer in the minibuffer using flex, only if we're in the minibuffer.
The benefit here is that the most recent buffers are suggested first."
  (when (not old)
    (setq he-expand-list '())
    ;; Are we looking for a buffer?
    (when (and (window-minibuffer-p)
	       ;; This is kind of hacky, but I don't know how to check otherwise.
	       (string-match-p "buffer" (minibuffer-prompt)))
      ;; This way doesn't work
      ;; (memq minibuffer-local-filename-completion-map
      ;; 	    (current-active-maps))
      (he-init-string (minibuffer-prompt-end) (point-max))
      (let ((flexpat (format "%s"
			     (apply #'concat
				    (-interpose ".*" (string-glyph-split he-search-string))))))
	(setq he-expand-list (->> (buffer-list)
				  (-map #'buffer-name)
				  (-filter (lambda (s) (string-match-p flexpat s)))
				  (-take 5))))))
  (while (and he-expand-list
	      (or (not (car he-expand-list))
		  (he-string-member (car he-expand-list) he-tried-table t)))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
	(if old (he-reset-string))
	())
    (progn
      (he-substitute-string (car he-expand-list) t)
      (setq he-expand-list (cdr he-expand-list))
      t)))


;;; Project filename expansion

;; TODO: Add the partial version.
;;;###autoload
(defun try-complete-project-file-name (old)
  "Try to complete text as a file name in the project."
  (unless old
    (when-let ((pr (project-current nil)))
      (he-init-string (he-file-name-beg) (point))
      (setq he-expand-list '())
      (save-match-data
	(dolist (prfile (project-files (project-current nil)))
	  (when (string-match (format "\\(%s.+\\)$" (regexp-quote he-search-string)) prfile)
	    (push (match-string 1 prfile) he-expand-list))))))

  (while (and he-expand-list
	      (or (not (car he-expand-list))
		  (he-string-member (car he-expand-list) he-tried-table t)))
    (setq he-expand-list (cdr he-expand-list)))

  (if (null he-expand-list)
      (progn
	(if old (he-reset-string))
	())
    (progn
      (he-substitute-string (car he-expand-list) t)
      (setq he-expand-list (cdr he-expand-list))
      t)))


;;; Visible buffers

(defun jmm-visible-symbol-completions (str)
  "Returns a list of visible symbols starting with STR.
Tries to sort by distance to window's cursor."
  (thread-last
    (cl-loop for win in (window-list)
	     nconc (with-selected-window win
		     (save-excursion
		       (let ((pos (window-point win))
			     (beg (window-start win))
			     (end (window-end win)))
			 (goto-char beg)
			 (cl-loop while (re-search-forward (rx-to-string `(seq bow ,str (1+ (or wordchar (syntax symbol))) eow)) end t)
				  collect (cons (match-string-no-properties 0)
						(abs (- (point) pos)))
				  )))))
    (seq-sort-by #'cdr #'<)
    (mapcar #'car)
    (seq-uniq)))

;; TODO: Also complete spaces.
;; FIXME: Incorrectly expands in the middle "like^this" will get expanded to "likethisthis".
;;;###autoload
(defun jmm-try-expand-dabbrev-visible (old)
  "Try to expand visible abbreviations dynamically.
Unlike `try-expand-dabbrev-visible', tries to weight based on distance."
  (unless old
    (progn
      (he-init-string (he-dabbrev-beg) (point))
      (setq he-expand-list (thread-first
			     he-search-string
			     (jmm-visible-symbol-completions)
			     (seq-difference he-tried-table)))))

  (while (and he-expand-list
	      (or (not (car he-expand-list))
		  (he-string-member (car he-expand-list) he-tried-table t)))
    (setq he-expand-list (cdr he-expand-list)))

  (if (null he-expand-list)
      (progn
	(if old (he-reset-string))
	())
    (progn
      (he-substitute-string (car he-expand-list) t)
      (setq he-expand-list (cdr he-expand-list))
      t)))


;;; Dictionary functions

(defun jmm-hippie-aspell-simple-word-list ()
  "Return a list of words from GNU aspell.
Only returns root words."
  (thread-first
    (shell-command-to-string "aspell dump master | aspell expand | aspell munch-list")
    (split-string "\n")
    (thread-last
      (mapcar (lambda (x) (car (split-string x "/"))))
      (seq-sort-by #'length #'<))))

(defun jmm-hippie-aspell-extended-word-list ()
  "Return a list of words from GNU aspell."
  (thread-first
    (shell-command-to-string "aspell dump master | aspell expand")
    (split-string "\n")
    (thread-last
      (seq-remove (lambda (x) (string-match-p "'s$" x))))))

(defvar jmm-hippie-simple-word-list
  (lazy-completion-table jmm-hippie-simple-word-list
			 (lambda ()
			   (jmm-hippie-aspell-simple-word-list)))
  "A lazy completion table of (simple) words.
Doesn't include affixes.")

(defvar jmm-hippie-extended-word-list
  (lazy-completion-table jmm-hippie-extended-word-list
			 (lambda ()
			   (jmm-hippie-aspell-extended-word-list)))
  "A lazy completion table of extended words. Includes affixes.")

(defvar jmm-hippie-dictionary-search-length-minimum 4
  "Minimum number of characters before trying to expand a word.")


;;; Word frequency functions
(defvar jmm-hippie-word-frequency-file (locate-user-emacs-file "hippie/count_1w.txt")
  "A file with words and their frequencies.
Download from https://norvig.com/ngrams/count_1w.txt")

(defvar jmm-hippie-word-frequency-table nil
  "Will be a hash table of words to their frequencies.")

(defun jmm-hippie--generate-word-frequency-hash-table ()
  "Create a hash table of words and their frequencies.
Uses https://norvig.com/ngrams/count_1w.txt, which should be locally downloaded."
  (let ((wordcounts (make-hash-table :test 'equal :size 500000))
	line idx)
    (with-temp-buffer
      (insert-file-contents-literally jmm-hippie-word-frequency-file)
      ;; For optimization purposes, I try to avoid copying the buffer string unnecessarily
      (while (not (eobp))
	;; It also seems like having a "let" inside "while" is slower than having it on the outside.
	(setq line (buffer-substring (line-beginning-position) (line-end-position))
	      idx (string-match "\t" line))
	(puthash (substring line 0 idx)
		 (string-to-number (substring line (1+ idx)))
		 wordcounts)
	(forward-line 1)))
    wordcounts))

(defun jmm-hippie--ensure-word-frequency-table ()
  "Initializes `jmm-hippie-word-frequency-table' if it's nil."
  (unless jmm-hippie-word-frequency-table
    (let ((reporter (make-progress-reporter "Generating word frequency table")))
      (setq jmm-hippie-word-frequency-table (jmm-hippie--generate-word-frequency-hash-table))
      (progress-reporter-done reporter))))

(defun jmm-hippie--sort-words-by-frequency (words)
  "Sort a list of WORDS by their descending frequency."
  (jmm-hippie--ensure-word-frequency-table)
  (seq-sort-by (lambda (word) (gethash word jmm-hippie-word-frequency-table 0)) #'> words))


;;; Dictionary expansion

;;; FIXME: Doesn't yet handle case correctly.
;;; Like, when you start with a capital letter.

;;;###autoload
(defun jmm-try-expand-dictionary-word (old)
  "Try to expand words, sorted by frequency"
  (unless old
    (progn
      (he-init-string (he-dabbrev-beg) (point))
      (unless (length< he-search-string jmm-hippie-dictionary-search-length-minimum)
	(setq he-expand-list (thread-first
			       (all-completions he-search-string jmm-hippie-extended-word-list)
			       (jmm-hippie--remove-tried)
			       (jmm-hippie--remove-low-effort)
			       (jmm-hippie--sort-words-by-frequency))))))
  (if (null he-expand-list)
      (progn
	(if old (he-reset-string))
	nil)
    (progn
      (he-substitute-string (car he-expand-list) t)
      (setq he-expand-list (cdr he-expand-list))
      t)))

(defun jmm-hippie--flex-dict-pattern (pattern)
  "Make a flex regular expression for PATTERN.
Words must start and end with same start and end as PATTERN.
Characters inside pattern must be in order."
  (thread-first
    pattern
    (string-glyph-split)
    ;; TODO: Integrate jmm-isearch
    (jmm-isearch--interleave-separator '(0+ not-newline))
    ((lambda (x) `(sequence bow ,@x eow)))
    (rx-to-string)))

(defun jmm-hippie--flex-dict-completions (pattern)
  "Returns flex dictionary expansions for PATTERN.
See `jmm-hippie--flex-dict-pattern'."
  (let* ((regexp (jmm-hippie--flex-dict-pattern pattern))
	 (pred (lambda (x) (string-match-p regexp x))))
    (seq-filter pred (all-completions "" jmm-hippie-extended-word-list))))

;;;###autoload
(defun jmm-try-expand-flex-dictionary-word (old)
  "Try to expand words in a \"flex\" style, sorting by word frequency.
Takes at most 3 guesses."
  (unless old
    (progn
      (he-init-string (he-dabbrev-beg) (point))
      (unless (length< he-search-string jmm-hippie-dictionary-search-length-minimum)
	(setq he-expand-list (thread-first
			       (jmm-hippie--flex-dict-completions he-search-string)
			       (jmm-hippie--remove-low-effort)
			       (jmm-hippie--remove-tried)
			       (jmm-hippie--sort-words-by-frequency)
			       (seq-take 3))))))
  (if (null he-expand-list)
      (progn
	(if old (he-reset-string))
	nil)
    (progn
      (he-substitute-string (car he-expand-list) t)
      (setq he-expand-list (cdr he-expand-list))
      t)))


;; (completing-read "Word: " jmm-hippie-simple-word-list)


;;; Limit completions to major mode

;;;###autoload
(defun jmm-maybe-try-complete-lisp-symbol-partially (old)
  (unless (derived-mode-p 'text-mode)
    (try-complete-lisp-symbol-partially old)))

;;;###autoload
(defun jmm-maybe-try-complete-lisp-symbol (old)
  (unless (derived-mode-p 'text-mode)
    (try-complete-lisp-symbol old)))

;;;###autoload
(defun jmm-maybe-try-expand-flex-dictionary-word (old)
  (when (derived-mode-p 'text-mode)
    (jmm-try-expand-flex-dictionary-word old)))

;;;###autoload
(defun jmm-maybe-try-expand-dictionary-word (old)
  (when (derived-mode-p 'text-mode)
    (jmm-try-expand-dictionary-word old)))

(provide 'hippie-expand-jmm)
;;; hippie-expand-jmm.el ends here
