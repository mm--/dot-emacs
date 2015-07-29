;; Various utilities to get Cangjie codes, pinyin pronunciation, and
;; definitions from a Chinese dictionary file
;; - Josh Moller-Mara

(defun josh/chinese-cangjie-code (str)
  "Get the cangjie code for a string"
  (interactive)
  (with-temp-buffer
    (set-input-method "Cangjie5")
    (mapconcat (lambda (x)
		 (upcase (mapconcat 'identity (quail-find-key (string-to-char x)) "/")))
	       (split-string str "" t)
	       " ")))

(defun josh/chinese-cangjie-codes (words)
  "Get the cangjie codes for traditional + simplified"
  (if (listp words)
      (format "%s\n[%s]" (josh/chinese-cangjie-code (cdr words))
	      (josh/chinese-cangjie-code (car words)))
    (josh/chinese-cangjie-code words)))

(defvar josh/chinese-dictionary-path
  (concat (file-name-directory load-file-name) "cedict_ts.u8")
  "Where we store the Chinese dictionary cedict_ts.u8")

(defun josh/chinese-prompt ()
  "Prompt for a character, return it"
  (setq josh/chinese-word (read-from-minibuffer "Word/Phrase: ")))


(defun josh/chinese-dict-find (phrase)
  "Find a chinese word or phrase in the dictionary"
  (with-temp-buffer
    (insert-file-contents josh/chinese-dictionary-path)
    (let (definitions)
      (while (re-search-forward (concat "^[^][]*\\b" phrase "\\b.*?$") nil t)
	(push (buffer-substring (match-beginning 0)
				(match-end 0))
	      definitions))
      (setq josh/chinese-word-dict
	   (if (equal (length definitions) 1)
	       (car definitions)
	     (helm-comp-read "Pick a definition: "
			     definitions
			     :nomark t))))))

(defun josh/chinese-get-pronunciation (dictentry)
  "Get a pronunciation from a dictionary entry."
  (let ((pronunciation (save-match-data
			 (and (string-match "\\[\\(.*?\\)\\]" dictentry)
			      (match-string 1 dictentry)))))
    (with-temp-buffer
      (set-input-method "chinese-sisheng")
      (mapconcat (lambda (x)
		   (let ((translation (quail-map-definition
					(quail-lookup-key x))))
			  (or (if (listp translation)
					(elt (cdr translation) 0)
				(char-to-string translation)) ;sisheng doesn't have 5th tone
			      (substring x 0 -1))))
		 (split-string pronunciation " " t)
		 " "))))

(defun josh/chinese-get-definition (dictentry)
  "Get a definition from a dictionary entry."
  (save-match-data
    (and (string-match "/\\(.*?\\)$" dictentry)
	 (mapconcat 'identity
		    (split-string (match-string 1 dictentry) "/" t)
		    "\n"))))

(defun josh/chinese-get-word (dictentry)
  "Return either the character, or a list of traditional and simplified."
  (let* ((words (save-match-data
		  (and (string-match "^\\(.+?\\) \\(.+?\\)\\b" dictentry)
		       (cons (match-string 1 dictentry)
			     (match-string 2 dictentry)))))
	 (traditional (car words))
	 (simplified (cdr words)))
    (if (equal traditional simplified)
	(setq josh/chinese-words traditional)
      (setq josh/chinese-words words)
      (format "%s [%s]" simplified traditional))))

(defun josh/chinese-def-at-point (&optional arg)
  "Get the definition of a character at the point and display in
the minibuffer. With an argument, insert the definition into the
buffer."
  (interactive "P")
  (let ((phrase (if (use-region-p)
		    (buffer-substring-no-properties (region-beginning) (region-end))
		  (string (char-after))))
	definitions)
    (with-temp-buffer
      (insert-file-contents josh/chinese-dictionary-path)
      (while (re-search-forward (concat "^[^][]*\\b" phrase "\\b.*?$") nil t)
	  (push (buffer-substring (match-beginning 0)
				  (match-end 0))
		definitions)))
    (let ((defs (mapconcat 'identity definitions "\n")))
	  (if arg
	      (insert defs)
	    (message defs)))))

(global-set-key (kbd "<f9> C") 'josh/chinese-def-at-point)

(defvar josh/chinese-decomposition-path
  (concat (file-name-directory load-file-name) "cjk-decomp-0.4.0.txt")
  "Where we store the Chinese character decomposition data")

(defun josh/chinese-decomposition-find (phrase)
  "Find a chinese word or phrase in the dictionary"
  (defun str-decomp (strnum)
    (if (and strnum (= (string-to-number strnum) 0))
	strnum
      (josh/chinese-decomposition-find strnum)))
  (with-temp-buffer
    (insert-file-contents josh/chinese-decomposition-path)
    (let (definitions)
      (when (re-search-forward (concat "^" phrase ":.*?$") nil t)
	(let ((decomp (buffer-substring (match-beginning 0)
					(match-end 0))))
	  (save-match-data
	    (string-match "^\\(.*?\\):\\(.*?\\)(\\(.*?\\))$" decomp)
	    (let* ((decomptype (match-string 2 decomp))
		   (constituents (split-string (match-string 3 decomp) "[,()]")))
	      (cons decomptype (mapcar 'str-decomp constituents)))))))))

(defun josh/chinese-decomposition-at-point ()
  "Get the decomposition of a character at the point and insert it."
  (interactive)
  (defun listtostr (x)
    (if (listp x)
	(mapconcat 'listtostr x "")
      x))
  (let ((phrase (string (char-after))))
    (insert (listtostr (josh/chinese-decomposition-find phrase)))))

(global-set-key (kbd "<f9> E") 'josh/chinese-decomposition-at-point)

(provide 'josh-chinese)
