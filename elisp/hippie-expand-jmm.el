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


;; Internal functions
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
	(regpat (apply #'concat
		       (-interpose
			;; FIXME: Other types of hyphens or word separators?
			"\\(\\s *\\|[-/]\\|\n\\s *\\)"
			(mapcar (lambda (char)
				  (format "\\b[%s%s]\\sw*\\b"
					  (regexp-quote (downcase char))
					  (regexp-quote (upcase char))))
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

;; DONE: Flex should only work for alpha characters. It should refuse to expand things like "tg."
(defun he-flex-init ()
  (he-init-string (he-dabbrev-beg) (point))
  (unless (string-match-p "^[-a-z]+$" he-search-string)
    (setq he-search-string "")))

;; Maybe the pattern should be created outside?
;; Not sure how often this runs, or if it matters.
;; TODO: Would be cool to have this do something different in the minibuffer. Like, only search symbols.
;; MAYBE: Allow multiword flex search by using periods.
(defun he-flex-search (pattern &optional reverse limit)
  "Like `he-dabbrev-search', but with a flex pattern.
Allows you to expand lisp symbols that contain a hyphen or slash."
  (let* ((result ())
	 (separator (if (eq major-mode 'emacs-lisp-mode)
			"[-/a-zA-Z]*" ;; Only allow slashes from Emacs Lisp.
		      "[-a-zA-Z]*"))
	 (regpat (format "\\b%s\\b"
			 (apply #'concat
				(-interpose separator (string-glyph-split pattern))))))
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
  (he--all-buffers2 old #'he-flex-init #'he-flex-search #'he-substitute-string-transcase))

;;;###autoload
(defun try-expand-flex-dabbrev (old)
    "A flexible type of dynamic abbreviation.
For example \"rblty\" could expand to \"reproducibility\".
Kind of like `try-expand-dabbrev'."
  (he--one-buffer old #'he-flex-init #'he-flex-search #'he-substitute-string-transcase))


;;; "dabbrev2"
;; Like "try-expand-dabbrev", but allows you to expand subsequent words with spaces.


(defvar he-dabbrev2--lastused nil
  "Was `try-expand-dabbrev-historical' the last one used?")
(defvar he-dabbrev2--last-location nil)

(defvar he-dabbrev2--last-expansion (make-marker))

;; TODO: If there's no previous expansion, search for the previous
;; word (I guess somewhere), then continue from there.


(defun he-dabbrev2-forward-loc ()
  (let (expansion)
    (save-excursion
      (save-restriction
	(if hippie-expand-no-restriction
	    (widen))
	(goto-char he-search-loc)
	;; Go forward over last place and spaces
	;; FIXME: There could just be a line break without spaces
	(re-search-forward "\\<\\(\\sw\\|\\s_\\)+\\([[:space:]]\\|\n\\)+" nil t)
	(set-marker he-search-loc (point))
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
	    (set-marker he-dabbrev2--last-expansion (point))
	    t)
	(progn
	  (set-marker he-search-loc nil)
	  (set-marker he-dabbrev2--last-expansion nil)
	  nil)
	))))


;;; Minibuffer expansion

;; TODO: Need to load bookmarks automatically

;; DONE: Maybe make it search bookmarks with files.
;; MAYBE: Require it ends in a comma to look for a bookmark.
;; Or like ",b" for bookmarks. ",p" for projects ",d" for open dired buffers.
;;;###autoload
(defun try-minibuffer-expand-bookmark (old)
  "Expand a bookmark in the minibuffer, only if we're in the minibuffer, and if it has a filename."
  (if (not old)
      ;; Are we looking for a file?
      (when (eq minibuffer-completion-table 'read-file-name-internal)
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

(provide 'hippie-expand-jmm)
;;; hippie-expand-jmm.el ends here
