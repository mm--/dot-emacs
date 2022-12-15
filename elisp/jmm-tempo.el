;;; jmm-tempo.el --- Josh's tempo templates and utilities  -*- lexical-binding: t; -*-

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

;; I use skeletons a lot, because they're documented in the manual.
;; I took a look at tempo templates, and they seem interesting, and
;; maybe more flexible than skeletons.

;; I like the fact that tempo uses markers to save positions
;; (skeleton's just uses positions, which can get obliterated), but I
;; don't like how it just never deletes markers.

;;; Code:
(require 'tempo)
(require 'abbrev)

(defun jmm-tempo-user-elements (arg)
  "Adds some more tempo elements.
See `tempo-define-template'.

New elements:

- '(EXPRESSION): Evaluates elisp EXPRESSION, but ignores the result.
- '(j:let BINDINGS BODY): Allows you to use tempo-named as variables in elisp BODY.
             See `jmm-tempo--let-tempo-named' for an example.
- '(j:set VARIABLE BODY): Allows you to set tempo-named as variable using in elisp BODY.
             See `jmm-tempo--set-tempo-named' for an example.
- '(j:prompt VARIABLE BODY): Runs and inserts BODY if VARIABLE is nil.
"
  (pcase arg
    ;; I liked that in skeletons you could do things like '(setq v1
    ;; "whatever") and it would run elisp but ignore the value.
    ;; I didn't notice an equivalent. I guess I could just do "(ignore
    ;; (my expression))" but that's an extra paren.
    (`(quote ,expr) (progn
		      (eval expr) ;; We need to return something, but if we
		      '(l nil)))  ;; return nil, tempo will think we
    ;; didn't handle this element.
    (`(j:let ,bindings . ,body) `(l ,(eval `(jmm-tempo--let-tempo-named ,bindings ,@body))))
    (`(j:set ,variable . ,body) (progn
				  (eval `(jmm-tempo--set-tempo-named ,variable ,@body))
				  `(l nil)))
    (`(j:prompt ,variable . ,body) (eval `(jmm-tempo--prompt-tempo-named ,variable ,@body)))
    (`(j:subtempo ,variable) `(l ,@(eval variable)))))

(add-to-list 'tempo-user-elements #'jmm-tempo-user-elements)

;; MAYBE: Add autoloads somehow?
;; TODO: Add command modes so it shows up in M-X.
;;;###autoload
(defmacro define-jmm-tempo (name documentation options &rest template)
  "Define a tempo template.
Like a cross between `define-skeleton' and `tempo-define-template'.

Lets you directly specify the NAME of the command (and variable) to insert the template.
Cleans up previous `tempo-marks'.

OPTIONS doesn't do anything yet, but will eventually allow you to specify buffer modes.
Will probably be a `declare' form in the future.
Might also let you specify some dynamically bound vars, like \"v1\" and \"v2\" for skeletons.
Just pass in nil or () for now though.
"
  (declare (doc-string 2)
           (indent defun))
  `(progn
     ;; Add 'no-self-insert (and return t) so if we're expanded as an
     ;; abbrev, we don't insert whatever caused the expansion.
     (put ',name 'no-self-insert t)
     (set ',name ',template)
     (defun ,name (&optional str arg)
       ,(concat documentation
		(if (string-match "\n\\'" documentation)
		    "" "\n")
		"\n"
		"This is a jmm-tempo template. See `define-jmm-tempo'.")
       (interactive nil)
       (jmm-tempo--clean-up-marks)
       (atomic-change-group
	 ;; Should I use `tempo-insert-region'?
	 (tempo-insert-template ',name nil)
	 ;; I think tempo-insert-template does return a marker but I'm
	 ;; explicitly returning t for 'no-self-insert abbreviation
	 ;; expansion
	 t))))

(defun jmm-tempo--clean-up-marks ()
  (mapc (lambda (mark) (set-marker mark nil)) tempo-marks)
  (setq tempo-marks nil))

(defun jmm-tempo-lookup-named (var)
  "The same as `tempo-lookup-named', but defines a `setf' form."
  (tempo-lookup-named var))

(gv-define-setter jmm-tempo-lookup-named
    (val name) `(setf (alist-get ,name tempo-named-insertions) ,val))

(defmacro jmm-tempo--let-tempo-named (bindings &rest body)
  "Bind tempo named locations to variables within BODY.
Example:
(jmm-tempo--let-tempo-name (v1 v2)
 (setf v1 \"Hi\")
 (message \"%s %s\" v1 v2))

Gets expanded to:
(progn
 (setf (`jmm-tempo-lookup-named' 'v1) \"Hi\")
 (message \"%s %s\" (jmm-tempo-lookup-named 'v1) (jmm-tempo-lookup-named 'v2)))

Should allow you to get and set named variables."
  (declare (indent 1))
  `(cl-symbol-macrolet ,(mapcar (lambda (var) `(,var (jmm-tempo-lookup-named ',var))) bindings)
     ,@body))

(defmacro jmm-tempo--set-tempo-named (variable &rest body)
  "Set tempo named location to evaluation of BODY.
Returns nil.

Example:
(jmm-tempo--set-tempo-name v1
 (message \"Hello\")
 \"Test\")

Gets expanded to something like:
(setf (`jmm-tempo-lookup-named' 'v1)
  (progn (message \"Hello\")
    \"Test))"
  (declare (indent 1))
  `(jmm-tempo--let-tempo-named
    (,variable)
    (setf ,variable (progn ,@body))))

;; TODO: Add NOINSERT parameter, like '(P) in `tempo-define-template'
(defmacro jmm-tempo--prompt-tempo-named (variable &rest body)
  "Insert VARIABLE, if it's non-nil. Otherwise, evaluate elisp BODY to set and insert variable.
If nothing's returned by the prompt, add a tempo mark.
Note, though, that the empty string \"\" still counts as something."
  (declare (indent 1))
  `(jmm-tempo--let-tempo-named
       (,variable)
     (or ,variable ;; If we have a saved value, just return it
	 (prog1	   ;; If we don't have anything set the new value, returning a list (can't return just nil.)
	     (list 'l (setf ,variable (progn ,@body)))
	   (unless ,variable ;; If the prompt actually returned nil, add a new mark.
	     (tempo-insert-mark (point-marker)))))))

(defmacro jmm-tempo-no-forgetting (&rest body)
  "Don't forget tempo named insertions while in BODY.
Allows sequential tempo templates to share the same named insertions.
(However, since `tempo-named-insertions' is buffer-local, it cannot be shared across buffers.)
Temporarily disables `tempo-forget-insertions', but runs it at the end."
  (declare (indent 0))
  `(unwind-protect
    (progn
      (add-function :override (symbol-function 'tempo-forget-insertions) #'ignore)
      ,@body)
    (remove-function (symbol-function 'tempo-forget-insertions) #'ignore)
    (tempo-forget-insertions)))

(defmacro jmm-tempo--add-bindings (variables &rest body)
  "Copy VARIABLES into `tempo-named-insertions' within BODY.
Kind of the opposite direction of `jmm-tempo--let-tempo-named'.
This is for passing variables from elisp to tempo,
whereas `jmm-tempo--let-tempo-named' is for passing variables from tempo to elisp.

For example:
(let ((v1 \"Hello\"))
  (jmm-tempo--add-bindings (v1)
    (some-tempo-template)))

some-tempo-template should see (s v1) as \"Hello\"."
  (declare (indent 1))
  `(let ((tempo-named-insertions (append tempo-named-insertions
					 (list ,@(mapcar (lambda (var) `(cons ',var ,var)) variables)))))
     ,@body))

;;;;;;;;;;

;;;###autoload
(defun jmm-add-local-tempo ()
  "Open this file, try to find the right place to add a tempo template.

Prompts for an abbreviation name, tempo name.
Leads you through the process of defining a tempo template.
Once you're done editing the template, evaluate it and `exit-recursive-edit'.
Then the abbreviation will be inserted in the `edit-abbrevs' buffer.
"
  (interactive)
  (let ((table-name (abbrev-table-name local-abbrev-table)))
    (find-library-other-window "jmm-tempo")
    (when table-name
      (goto-char (point-min))
      (let ((markstring (concat "(" (symbol-name table-name) ")")))
	(if (search-forward markstring nil t)
	    (progn
	      (forward-page)
	      (forward-line 0)
	      (open-line 3))
	  (progn
	    (goto-char (point-max))
	    (backward-page)
	    (forward-line 0)
	    (dlet ((tempo-named-insertions `((mark1 . ,markstring))))
	      (jmm-tempo/emacs-lisp/tempo-boundary))))
	(let* ((tabbrev (read-string "Abbreviation: "))
	       (ttable table-name)
	       (tdefault (jmm-tempo--default-name ttable tabbrev))
	       (tname (read-string (format-prompt "Tempo name" tdefault)
				   nil nil tdefault)))
	  (jmm-tempo--add-bindings (tname)
	    (jmm-tempo/emacs-lisp/jmm-tempo-abbrev))
	  (recursive-edit)
	  (jmm-tempo-edit-abbrevs-for table-name)
	  (insert "\n")
	  (jmm-tempo--add-bindings (tname tabbrev)
	    (jmm-tempo/edit-abbrevs/last-tempo)))))))

(defun jmm-tempo-edit-abbrevs-for (table-name)
  "Edit abbreviations for a given TABLE-NAME.
Basically just `edit-abbrevs' for a provided table. "
  (let* ((buf (prepare-abbrev-list-buffer)))
    (pop-to-buffer-same-window buf)
    (when (and table-name
               (search-forward
                (concat "(" (symbol-name table-name) ")\n\n") nil t))
      (goto-char (match-end 0)))))

;;;###autoload
(defun jmm-tempo-yank-template ()
  "Insert current kill as a series of strings in tempo."
  (interactive nil emacs-lisp-mode)
  (atomic-change-group
    (thread-first
      (current-kill 0)
      (substring-no-properties)
      (split-string "\n")
      (thread-last
	(mapcar #'string-trim)
	(mapc (lambda (line)
		(insert (prin1-to-string line))
		(indent-according-to-mode)
		(insert " n> \n")))))))
(put 'jmm-tempo-yank-template 'no-self-insert t)


;;;;;;;;;;
;; (emacs-lisp-mode-abbrev-table)

;;;###autoload (autoload 'jmm-tempo/emacs-lisp/jmm-tempo "jmm-tempo")
(define-jmm-tempo jmm-tempo/emacs-lisp/jmm-tempo
  "A `define-jmm-tempo' template for making `define-jmm-tempo' templates."
  (declare (modes emacs-lisp))
  "(define-jmm-tempo " (P "Name: " tname) n>
  (prin1-to-string (read-string "Documentation: ")) n>
  "nil" n>
  p ")" >)

(define-jmm-tempo jmm-tempo/emacs-lisp/tempo-boundary
  "Boundaries between different abbreviation table groups"
  nil
  (make-string 1 ?\f) n
  ";;;;;;;;;;" n
  ";; " (P "Mark string?"  mark1) n n
  p
  n n n)

(defun jmm-tempo--default-name (table-name abbr)
  "Suggest a default tempo name for a local abbrevation table and abbreviation."
  (format
   "jmm-tempo/%s/%s"
   (thread-last
     (symbol-name table-name)
     (string-remove-suffix "-abbrev-table")
     (string-remove-suffix "-mode"))
   abbr))

;;;###autoload (autoload 'jmm-tempo/emacs-lisp/jmm-tempo-abbrev "jmm-tempo")
(define-jmm-tempo jmm-tempo/emacs-lisp/jmm-tempo-abbrev
  "Prompt for an abbreviation. Make a tempo template."
  nil
  (j:subtempo jmm-tempo/emacs-lisp/autoload2)
  n
  (j:subtempo jmm-tempo/emacs-lisp/jmm-tempo))

;;;###autoload (autoload 'jmm-tempo/emacs-lisp/letsexp "jmm-tempo")
(define-jmm-tempo jmm-tempo/emacs-lisp/letsexp
  "Encase the next s-expression in a let form."
  (declare (modes emacs-lisp))
  "(let ((" (P "Variable: " var) " "
  '(let ((start (point)))
    (forward-sexp)
    (indent-region start (point) nil))
  "))" n>
  p (s var) ")" >)

;;;###autoload (autoload 'jmm-tempo/emacs-lisp/autoload2 "jmm-tempo")
(define-jmm-tempo jmm-tempo/emacs-lisp/autoload2
  "Make an explicit autoload cookie"
  nil
  ";;;###autoload (autoload '"
  (j:prompt tname (completing-read "Symbol: " obarray))
  " "
  (prin1-to-string (file-name-sans-extension (file-name-nondirectory buffer-file-name)))
  ")")


;;;;;;;;;;
;; (jmm-xhtml-mode-abbrev-table)

;; MAYBE: Truncate string to some length?
;; MAYBE: Guarantee uniqueness?
(defun jmm-tempo--string-to-xml-id (string)
  "Change an arbitrary string to something that could be an XML id."
  (thread-last
    string
    downcase
    (replace-regexp-in-string (rx (1+ (not alphanumeric))) " ")
    (string-trim)
    (replace-regexp-in-string (rx (1+ (not alphanumeric))) "-")))

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/section "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/section
  "Insert an HTML section."
  nil
  (P "Header level: " header t)
  (P "Header title: " title t)
  (j:let (id title)
	 (setf id (jmm-skeleton-prompt "ID: " (list (jmm-tempo--string-to-xml-id title))))
	 nil)
  "<section" (j:let (id)
		    (when id
		      (format " id=\"%s\"" (xml-escape-string id))))
  ">" n>
  "<"  (s header) ">" (s title) "</" (s header) ">" n>
  r> n>
  "</section>" >)

(define-jmm-tempo jmm-tempo/jmm-xhtml/header
  "Adds a header"
  nil
  "<" (P "Header: " header) ">" p "</" (s header) ">")


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/figkill "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/figkill
  "Make a figure from image path in kill ring"
  nil
  "<figure" (P "ID: "  id 'noinsert)
  (j:let (id)
	 (when id
	   (format " id=\"%s\"" (xml-escape-string id))))
  ">" n>
  "<img src=\"" (xml-escape-string (file-relative-name (expand-file-name (current-kill 0)))) "\"/>" n>
"<figcaption>" n>
 (P "Caption: " caption) > n>
"</figcaption>" > n>
"</figure>" >)

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/stht "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/stht
  "A span for a thought"
  nil
  "<span class=\"thought\" aria-hidden=\"true\" data-last-edited=\"" (xml-escape-string (format-time-string "%Y-%m-%d %H:%M")) "\">"
  r
  "</span>" >)


(defun jmm-tempo--xml-timestamp ()
  "Return a timestamp, XML escaped."
  (xml-escape-string (format-time-string "%Y-%m-%d %H:%M")))

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/dnote "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/dnote
  "Add a div note"
  nil
  "<div class=\"note\" aria-hidden=\"true\" data-last-edited=\"" (jmm-tempo--xml-timestamp) "\">" n>
  r> n>
  "</div>" >)


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/scite "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/scite
  "Add a span todo to make a citation"
  nil
  "<span class=\"cite\" aria-hidden=\"true\" data-last-edited=\"" (jmm-tempo--xml-timestamp) "\">" r> "</span>")

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/sdraft "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/sdraft
  "Add a draft span."
  nil
  "<span class=\"draft\" aria-hidden=\"true\" data-last-edited=\"" (jmm-tempo--xml-timestamp) "\">" r> "</span>")


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/sindex "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/sindex
  "Add a span for an indexed term"
  nil
  (P "Text: " term t)
  "<span class=\"index\""
  (j:let (term)
	 (when-let ((idx (jmm-skeleton-prompt "Indexed term: " (list term))))
	   (format " data-index=\"%s\"" (xml-escape-string idx))))
  ">" p (s term) "</span>")


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/mathil "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/mathil
  "MathML inline"
  nil
  "<math display=\"inline\" xmlns=\"http://www.w3.org/1998/Math/MathML\">" n>
  r> n>
  "</math>" >)


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/idkill "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/idkill
  "Add an id= attribute from current kill"
  nil
  "id=\"" p (jmm-tempo--string-to-xml-id (current-kill 0)) "\"")


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/ddraft "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/ddraft
  "Add a draft div"
  nil
  "<div class=\"draft\" aria-hidden=\"true\" data-last-edited=\"" (jmm-tempo--xml-timestamp) "\">" n>
  r> n>
  "</div>" >)


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/ruby1 "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/ruby1
  "Template for <ruby> HTML element"
  nil
  "<ruby>" r> (P "Ruby base: ") "<rp>(</rp><rt>" p (P "Ruby text: ") "</rt><rp>)</rp>" p  "</ruby>")



;;;;;;;;;;
;; (edit-abbrevs-mode-abbrev-table)

;;;###autoload (autoload 'jmm-tempo/edit-abbrevs/last-tempo "jmm-tempo")
(define-jmm-tempo jmm-tempo/edit-abbrevs/last-tempo
  "Make an abbreviation for the last tempo templaate"
  nil
  "\"" (P "Abbrev: " tabbrev) "\" 0 \"\" " (P "Tempo template: " tname) p %)


;;;;;;;;;;
;; (css-mode-abbrev-table)

;;;###autoload (autoload 'jmm-tempo/css/keyfrm "jmm-tempo")
(define-jmm-tempo jmm-tempo/css/keyfrm
  "Make CSS keyframes"
  nil
  "@keyframes " (P "Animation name: " name) " {" n>
  "from {" n>
  p > n>
  "}" > n>
  "to {" n>
  p > n>
  "}" > n>
  "}" > )


;;;;;;;;;;
;; (js2-mode-abbrev-table)

;;;###autoload (autoload 'jmm-tempo/js2/pthn "jmm-tempo")
(define-jmm-tempo jmm-tempo/js2/pthn
  "Promise \"then\""
  nil
  "then((" (P "Argument: " name) ") => {" n>
  p n>
  "})" >
  )



(provide 'jmm-tempo)
;;; jmm-tempo.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("jt-" . "jmm-tempo-"))
;; End:
