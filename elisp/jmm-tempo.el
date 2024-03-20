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

;; This file defines a lot of different tempo elements, which you can
;; use via `tempo-user-elements'.  However, it may be easier to simply
;; use Emacs Lisp shorthands and macros, which allow for nicer indenting.
;; If you use the shorthand ("jte:" . "jmm-tempo-element:"), you can have things like
;; 
;;   (jte:if-first some-id
;;       "first"
;;     "second" )
;;
;; indent nicely.  Also, you can more easily jump to the definition of
;; the macro.

;; To-dos:
;; - [X] Make a while loop, like subskeletons.  (Like, make it easier
;;       to make "<kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>I</kbd>")
;; - [ ] Make a timer counter variable?
;; 	 Might be able to do it by propertizing a string.
;; 	 (propertize "hello" 'jte-repeat 1)

;;; Code:
(require 'tempo)
(require 'abbrev)

(defvar jmm-tempo-use-region nil
  "Whether we should use the region, setting `tempo-region-start' and `tempo-region-stop'.
This is dynamically bound, letting new elements know whether to use the region.
Similar to `tempo-insert-template'’s “on-region”.")

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

;; TODO: Remove this in favor of macros and shorthands.
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
	 (let ((jmm-tempo-use-region nil))
	   (jmm-tempo--initialize-region)
	  ;; Should I use `tempo-insert-region'?
	   (tempo-insert-template ',name nil))
	 ;; I think tempo-insert-template does return a marker but I'm
	 ;; explicitly returning t for 'no-self-insert abbreviation
	 ;; expansion
	 t))))

(defun jmm-tempo--clean-up-marks ()
  (mapc (lambda (mark) (set-marker mark nil)) tempo-marks)
  (setq tempo-marks nil))

(defun jmm-tempo--initialize-region ()
  "Set the value of `jmm-tempo-use-region', `tempo-region-start' and `tempo-region-stop'.
TODO: Maybe use an argument like the current prefix?"
  (if (use-region-p)
      (progn
	(set-marker tempo-region-start (region-beginning))
	(set-marker tempo-region-stop (region-end))
	(setf jmm-tempo-use-region t))
    (set-marker tempo-region-start nil)
    (set-marker tempo-region-stop nil)
    (setf jmm-tempo-use-region nil)))

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

(defmacro jmm-tempo-element:set (variable &rest body)
  "See `jmm-tempo--set-tempo-named'.

Example:
(jte:set v1
 (message \"hello\")
 \"test\")

Would set \"v1\" \"test\"."
  (declare (indent 1))
  `(progn
     ,`(jmm-tempo--set-tempo-named ,variable ,@body)
     '(l nil)))

(defmacro jmm-tempo-util:get (variable)
  "Get value of unquoted variable.
For use inside lisp code.

Example:
(P \"Title: \" title t)
(format \"Hello %s\" (jtu:get v1))"
  `(jmm-tempo-lookup-named ',variable))

(defmacro jmm-tempo-element:do (&rest body)
  "Run the body, ignoring the result."
  `(progn
     ,@body
     '(l nil)))

(defun jmm-tempo--empty-p (x)
  "Is X nil or the empty string?"
  (or (null x)
      (and (stringp x) (string-empty-p x))))

(defmacro jmm-tempo-element:when-some (variable &rest body)
  "When VARIABLE is non-empty, run tempo BODY.
Non-empty here means non-nil and not the empty string.
BODY is executed with tempo.

Example:
(jt:when-some v1
 \"Hello\" n> \"there\")"
  (declare (indent 1))
  `(progn
     (if (jmm-tempo--empty-p (jmm-tempo-lookup-named ',variable))
	 '(l nil)
       (jte:bind (,variable) ,@body))))

(cl-defmacro jmm-tempo-element:if-set ((name expression) then &rest else)
  "Set NAME to result of lisp EXPRESSION, running template element THEN if non-empty, otherwise ELSE.

Example:
(jte:if-set (v1 (read-string \"Something: \"))
  (l \"I am inserting \" (s v1) \".\")
\"Nothing \" \"is \" \"set.\")"
  (declare (indent 2))
  `(if (progn
	 (jte:set ,name ,expression)
	 (not (jmm-tempo--empty-p (jmm-tempo-lookup-named ',name))))
       '(l ,then)
     '(l ,@else)))

(cl-defmacro jmm-tempo-element:when-set ((name expression) &rest body)
  "Set NAME to result of lisp EXPRESSION, running template BODY if non-empty.

Example:
(jte:when-set (v1 (read-string \"Something: \"))
\"I am inserting \" (s v1) \".\")"
  (declare (indent 1))
  `(if (progn
	 (jte:set ,name ,expression)
	 (not (jmm-tempo--empty-p (jmm-tempo-lookup-named ',name))))
       '(l ,@body)
     '(l nil)))

(defmacro jmm-tempo-element:bind (bindings &rest body)
  "Bind tempo named locations to variables within BODY.
Returned elements are executed with tempo.
(Returns a list (l ...body))"
  (declare (indent 1))
  `(cl-symbol-macrolet ,(mapcar (lambda (var) `(,var (jmm-tempo-lookup-named ',var))) bindings)
     (list 'l ,@body)))

(defmacro jmm-tempo-element:if-some (variable then &rest else)
  "If VARIABLE is non-empty, run tempo THEN otherwise ELSE.
Non-empty here means non-nil and not the empty string.
THEN and ELSE are executed with tempo.

Example:
(jt:if-some v1
 (s v1)
\"Empty\")"
  (declare (indent 2))
  `(progn
     (if (not (jmm-tempo--empty-p (jmm-tempo-lookup-named ',variable)))
	 '(l ,then)
       '(l ,@else))))

(defmacro jmm-tempo-element:while-some (variable start &rest body)
  "Runs BODY while template START sets VARIABLE.

Example:
(jte:while-some v1 (jte:set v1 (read-string \"Something: \"))
\"I am inserting \" (s v1) \".\")

Note: Don't use (P \"something\" v1 t), as it will never
reevaluate v1 after it's already set.
"
  (declare (indent 2))
  `(progn
     (while (progn
		  (tempo-insert ',start nil)
		  (not (jmm-tempo--empty-p (jmm-tempo-lookup-named ',variable))))
       (mapc (lambda (elt) (tempo-insert elt nil)) ',body))
     '(l nil)))

(cl-defmacro jmm-tempo-element:while-set ((variable expression) &rest body)
  "Runs BODY while lisp EXPRESSION returns a non-empty string for VARIABLE.

Example:
(jte:while-set (v1 (read-string \"Something: \"))
\"I am inserting \" v1 \".\")"
  (declare (indent 1))
  `(progn
     (while (progn
		  (jte:set ,variable ,expression)
		  (not (jmm-tempo--empty-p (jmm-tempo-lookup-named ',variable))))
       (mapc (lambda (elt) (tempo-insert elt nil)) ',body))
     '(l nil)))

(cl-defmacro jmm-tempo-element:if-first (variable first &rest rest)
  "Run template element FIRST the first time otherwise run REST.
Sets VARIABLE to the string \"jte:repeat\", unless you set it within FIRST.

Example:
(jte:if-first some-id \"\" \", \")"
  (declare (indent 2))
  `(progn
     (if (jmm-tempo--empty-p (jmm-tempo-lookup-named ',variable))
	 (progn (tempo-insert ',first nil)
		;; Check to see if it got set in FIRST?
		(unless (jmm-tempo--empty-p (jmm-tempo-lookup-named ',variable))
		  (jte:set ,variable "jte:repeat")))
       (mapc (lambda (elt) (tempo-insert elt nil)) ',rest))
     '(l nil)))

(cl-defmacro jmm-tempo-element:on-repeat (variable &rest body)
  "Run BODY when VARIABLE is initialized.
See `jmm-tempo-element:if-first'.

Example:
(jte:on-repeat repeat1 \", \")"
  (declare (indent 1))
  `(jte:if-first ,variable nil ,@body))

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
;; Other internal functions

(defun jmm-tempo--delete-and-return-line ()
  "Delete the current line (from beginning of indentation) and return a trimmed version."
  (let ((beg (save-excursion
	       (back-to-indentation)
	       (point)))
	(end (line-end-position)))
    (prog1
	(string-trim (buffer-substring-no-properties beg end))
      (delete-region beg end))))


;;;;;;;;;;
;; Some example templates

(define-jmm-tempo jmm-tempo/examples/while-example
  "An example of repeating things, setting a comma between elements."
  nil
  (jte:while-set (id (read-string "Enter an ID: "))
    (jte:on-repeat repeat1 "," " ")
    "ID:" (s id))
  n
  (jte:if-some repeat1
      "Added things!"
    "Nothing added."))


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

;;;###autoload (autoload 'jmm-tempo/emacs-lisp/letstar "jmm-tempo")
(define-jmm-tempo jmm-tempo/emacs-lisp/letstar
  "Insert a let* expression"
  nil
  "(let* ((" p "))" n>
  p
  ")" >)

;;;###autoload (autoload 'jmm-tempo/emacs-lisp/string-split "jmm-tempo")
(define-jmm-tempo jmm-tempo/emacs-lisp/string-split
  "Split a string with quotes, put cursor in middle"
  nil
  "\" " p " \""
  )


;;;###autoload (autoload 'jmm-tempo/emacs-lisp/string-split-quote "jmm-tempo")
(define-jmm-tempo jmm-tempo/emacs-lisp/string-split-quote
  "Split a string using quotes, adding escaped quotes on the inside."
  nil
  "\\\"\" " p " \"\\\""
  )



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


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/sidxa "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/sidxa
  "Automatically make an indexed term from kill ring."
  nil
  (j:set term (current-kill 0))
  "<span class=\"index\""
  (j:let (term)
	 (format " data-index=\"%s\"" (xml-escape-string term)))
  ">" p (s term) "</span>"
  )

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


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/spaul "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/spaul
  "Note from Paul"
  nil
  "<span class=\"paul\" data-datetime=\"" (xml-escape-string (format-time-string "%Y-%m-%d %H:%M")) "\">" p "</span>" >
  )


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/figrel "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/figrel
  "Make a figure with an image whose src is a relative path to the current killed text"
  nil
  "<figure>" n>
  "<img src=\"" (xml-escape-string (file-relative-name (expand-file-name (current-kill 0)))) "\"/>" n>
  "<figcaption>" p "</figcaption>" n>
  "</figure>" >
)


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/sec2a "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/sec2a
  "Make an h2 section, automatically add an ID"
  nil
  (P "Header title: " title t)
  (j:let (id title)
	 (setf id (jmm-tempo--string-to-xml-id title))
	 nil)
  "<section id=\"" (s id) "\">" n>
  "<h2>" (s title) "<a class=\"anchor\" href=\"#" (s id) "\"></a>" "</h2>" n>
  r> n>
  "</section>" >
  )

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/sec3a "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/sec3a
  "Make an h3 section, automatically add an ID"
  nil
  (P "Header title: " title t)
  (j:let (id title)
	 (setf id (jmm-tempo--string-to-xml-id title))
	 nil)
  "<section id=\"" (s id) "\">" n>
  "<h3>" (s title) "<a class=\"anchor\" href=\"#" (s id) "\"></a>" "</h3>" n>
  r> n>
  "</section>" >
)

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/h3a "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/h3a
  "Make an h3 header with an anchor"
  nil
  (P "Header title: " title t)
  (j:let (id title)
	 (setf id (jmm-tempo--string-to-xml-id title))
	 nil)
  "<h3 id=\"" (s id) "\">" (s title) " <a class=\"anchor\" href=\"#" (s id) "\"></a>" "</h3>")


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/detl "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/detl
  "A <details> tag, but makes the current line the summary"
  nil
  (ignore (jmm/nxml-blockify-element))
  (jt:set l1 (jt--delete-and-return-line))
  "<details>" n>
  "<summary>" (s l1) "</summary>" n>
  p > n>
  "</details>" >
  )

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/detlo "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/detlo
  "Details, but open"
  nil
  (jt:set l1 (jt--delete-and-return-line))
  "<details open=\"open\">" n>
  "<summary>" (s l1) "</summary>" n>
  p > n>
  "</details>" >
  )


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/kbd1 "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/kbd1
  "Enter a keyboard key"
  nil
  "<kbd>" (P "Key: ") "</kbd>"
)


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/imgrel "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/imgrel
  "Insert a relative image.  Takes an absolute path in the kill ring and makes an img tag."
  nil
  "<img src=\"" (xml-escape-string (file-relative-name (expand-file-name (current-kill 0)))) "\"/>" >
  )

(defun jmm-paragraph-to-sentences (str)
  "Convert paragraph STR to a list of string sentences."
  (cl-labels ((sentence-start ()
		(forward-sentence 1)
		(backward-sentence 1)
		(point))
	      (next-sentence ()
		(forward-sentence 1)
		(sentence-start)))
    (let (lastpoint)
      (with-temp-buffer
	(insert str)
	(goto-char (point-min))
	(save-excursion
	  (while (re-search-forward (rx "*" (group (minimal-match (1+ nonl))) "*") nil t)
	    (replace-match "<em>\\1</em>")))
	(sentence-start)
	(cl-loop collect (progn
			   (setq lastpoint (point))
			   (thing-at-point 'sentence))
		 until (eq (next-sentence) lastpoint))))))

(defun jmm-interleave (separators lst)
  "Interleave list SEPARATORS between elements of list LST."
  (let (notfirst)
    (cl-loop for x in lst
	     append (if notfirst
			`(,@separators ,x)
		      (setq notfirst t)
		      `(,x)))))

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/kps1 "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/kps1
  "Killed paragraph to sentences.  Converts lightly-formatted text to separate lines."
  nil
  (progn
    `(l ,@(thread-last (jmm-paragraph-to-sentences (current-kill 0))
		       (jmm-interleave `(> n)))
	>
	)))


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/cdat "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/cdat
  "Create a CDATA section."
  nil
  "<![CDATA[" p "]]>")


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/prek "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/prek
  "Code block from current kill."
  nil
  "<pre xml:space=\"preserve\">"
  (xml-escape-string (current-kill 0))
  "</pre>")

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/fontify-kill-html "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/fontify-kill-html
  "Fontify the current kill, returning HTML in a <code> block."
  nil
  "<code class=\"fontify\">"
  (car (jmm-htmlfontify-string (current-kill 0)))
  "</code>")

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/fontify-kill-html-pre-code "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/fontify-kill-html-pre-code
  "Take current kill, htmlfontify it, and return a pre code block"
  nil
  "<pre class=\"code\" xml:space=\"preserve\"><code class=\"fontify\">"
  (car (jmm-htmlfontify-string (current-kill 0) t))
  "</code></pre>")


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/fontify-kill-html-pre-samp "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/fontify-kill-html-pre-samp
  "Fontify the current kill as a <samp> element in a <pre>"
  nil
  "<pre class=\"samp\" xml:space=\"preserve\"><samp class=\"fontify\">"
  ;; TODO: Replace <span class="comint-highlight-input"> with <kbd>
  (car (jmm-htmlfontify-string (current-kill 0) t))
  "</samp></pre>"
  )

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/fontify-kill-code-inline "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/fontify-kill-code-inline
  "Fontify the kill, insert as an inline <code> element.
Use when you don’t want a separate block."
  nil
  "<code class=\"fontify\" xml:space=\"preserve\">"
  (car (jmm-htmlfontify-string (current-kill 0) t))
  "</code>"
  )

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/xhtmlpage "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/xhtmlpage
  "A basic XHTML page with some metadata"
  nil
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>" n>
  "<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en-US\"" n>
  "xmlns:dc=\"http://purl.org/dc/terms/\">" n>
  "<head>" n>
  "<meta charset=\"UTF-8\"/>" n>
  "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/>" n>
  "<meta name=\"author\" content=\"Josh Moller-Mara\"/>" n>
  "<meta name=\"dc:created\" content=\"" (format-time-string "%Y-%m-%d") "\"/>" n>
  "<title>" (P "Title: " title) "</title>" n>
  "<link rel=\"canonical\" href=\"" (P "Canonical url: ") "\"/>" n>
  "<link rel=\"stylesheet\" href=\"" (P "Stylesheet: ") "\" type=\"text/css\" title=\"Default style\"/>" n>
  "<script src=\"http://localhost:8081/skewer\"></script>" n>
  "</head>" n>
  "<body>" n>
  "<header>" n>
  "<h1>" (s title) "</h1>" n>
  "</header>" n>
  "<main>" n>
  "</main>" n>
  "<footer>" n>
  "<p>" n>
  "Last updated: <time property=\"dc:modified\" datetime=\"" (format-time-string "%Y-%m-%d") "\">" (format-time-string "%Y-%m-%d") "</time>" n>
  "</p>" n>
  "</footer>" n>
  "</body>" n>
  "</html>" n> )

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/dtdd "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/dtdd
  "Definition term and definition <dt> and <dd>"
  nil
  "<dt>" p "</dt>" n>
  "<dd>" n>
  p > n>
  "</dd>" > p )

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/div-dd-dt "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/div-dd-dt
  "Insert a div, then a dt, then a dd."
  nil
  "<div>" n>
  "<dt>" p "</dt>" n>
  "<dd>" n>
  p > n>
  "</dd>" > n>
  "</div>" > n> p
  )


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/pnew "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/pnew
  "New <p> with id, last edited attribute, and \"tofix\" class."
  nil
  "<p class=\"tofix\" data-last-edited=\"" (xml-escape-string (format-time-string "%Y-%m-%d %H:%M")) "\" id=\"" (jmm-xhtml--gen-new-id) "\">" n>
  p r> > n
  "</p>" > )

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/p-new-d "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/p-new-d
  "Like `jmm-tempo/jmm-xhtml/pnew' but makes a draft."
  nil
  "<p class=\"draft tofix\" data-last-edited=\"" (xml-escape-string (format-time-string "%Y-%m-%d %H:%M")) "\" id=\"" (jmm-xhtml--gen-new-id) "\" aria-hidden=\"true\">" n>
  p r> > n
  "</p>" >
  )


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/1lc "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/1lc
  "List element and cite"
  nil
  "<li><cite>" (P "Cite: ") "</cite></li>")

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/1time "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/1time
  "Insert a <time> with a datetime attribute"
  nil
  "<time datetime=\"" (j:prompt date (format-time-string "%Y-%02m-%02d" (org-read-date nil t nil "Date: "))) "\">" (s date) "</time>")

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/make-tooltip "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/make-tooltip
  "Wrap the region with a tooltip and a thought"
  nil
  ;; TODO: Figure out cleaner way to wrap region.  Maybe interregions in general.
  (jte:set v1 (point-marker))
  '(goto-char (region-beginning))
  "<span class=\"tooltip\">"
  '(goto-char (jtu:get v1))
  (j:subtempo jmm-tempo/jmm-xhtml/stht)
  "</span>")


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/a-figure-reference "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/a-figure-reference
  "Make a link that refers to a specific figure and its number"
  nil
  (jte:set id (current-kill 0))
  (jte:set figinfo (jmm-xhtml--get-figure-info-from-kill))
  "<a href=\"" (s id) "\" class=\"figurelink\">Figure " (jte:bind (figinfo) (alist-get 'number figinfo))"</a>")


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/a-anchor "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/a-anchor
  "Embed an anchor link pointing to ancestor with ID."
  nil
  (jte:set ancestor (jmm-xhtml--get-ancestor-id))
  '(delete-horizontal-space t)
  "<a class=\"anchor\" href=\"#" (s ancestor) "\"></a>" )



;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/rp1 "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/rp1
  "Repeat last element, inline"
  nil
  (ignore (jmm/nxml-wrap (save-excursion (nxml-backward-element) (xmltok-start-tag-qname)))))


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/akill-with-title "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/akill-with-title
  "Insert a link using the current kill. Add a title attribute with the title of the page."
  nil
  (jte:set urlstr (current-kill 0))
  (jte:set title (or (jmm--get-url-html-title (jtu:get urlstr)) ""))
  "<a href=\"" (xml-escape-string (jtu:get urlstr)) "\" title=\"" (xml-escape-string (jtu:get title)) "\">"
  (jte:set url (ignore-errors (url-generic-parse-url (jtu:get urlstr))))
  p
  (jte:bind (urlstr url title)
  (jmm-skeleton-prompt "Link text: "
			(seq-uniq
			 (seq-remove
			  #'null
			  (list
			   (xml-escape-string urlstr)
			   (ignore-errors (url-host url))
			   (ignore-errors (url-domain url))
			   title)))))
  "</a>"
  )

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/akill-external "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/akill-external
  "Insert link from current kill. Mark it as external (with text class)."
  nil
  (jte:set urlstr (current-kill 0))
  (jte:set title (or (jmm--get-url-html-title (jtu:get urlstr)) ""))
  "<a href=\"" (xml-escape-string (jtu:get urlstr)) "\" title=\"" (xml-escape-string (jtu:get title)) "\" class=\"external text\" rel=\"external\" data-retrieved=\"" (format-time-string "%Y-%m-%d") "\">"
  (jte:set url (ignore-errors (url-generic-parse-url (jtu:get urlstr))))
  p
  (jte:bind (urlstr url title)
  (jmm-skeleton-prompt "Link text: "
			(seq-uniq
			 (seq-remove
			  #'null
			  (list
			   (xml-escape-string urlstr)
			   (ignore-errors (url-host url))
			   (ignore-errors (url-domain url))
			   title)))))
  "</a>"

  )

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/akill-external-notitle "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/akill-external-notitle
  "Make external <a href>, but without prefetching the title."
  nil
  (jte:set urlstr (current-kill 0))
  "<a href=\"" (xml-escape-string (jtu:get urlstr)) "\" class=\"external text\" rel=\"external\" data-retrieved=\"" (format-time-string "%Y-%m-%d") "\">"
  (jte:set url (ignore-errors (url-generic-parse-url (jtu:get urlstr))))
  p
  (jte:bind (urlstr url title)
    (jmm-skeleton-prompt "Link text: "
			 (seq-uniq
			  (seq-remove
			   #'null
			   (list
			    (xml-escape-string urlstr)
			    (ignore-errors (url-host url))
			    (ignore-errors (url-domain url))
			    title)))))
  "</a>"
  )



;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/details-to-end "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/details-to-end
  "Create details where current line is summary and all following content is enclosed"
  nil
  (jte:set l1 (jt--delete-and-return-line))
  "<details open=\"open\">" n>
  "<summary>" (s l1) "</summary>" >
  (ignore
   (forward-line 1)
   ;; Hmm... it doesn't seem to exactly stay at the beginning of the
   ;; next line.  Probably has to do with the marker type used by
   ;; tempo
   (back-to-indentation))
  p
  (jte:set p1 (point))
  (ignore (jmm/nxml-end-of-inner-sexp))
  n>
  (jte:set p2 (point))
  "</details>" >
  (ignore
   (indent-region (jtu:get p1) (jtu:get p2)))
  )


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/pre-code-kill "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/pre-code-kill
  "Make a <pre><code> block from kill."
  nil
  "<pre class=\"code\" xml:space=\"preserve\"><code>"
  (xml-escape-string (current-kill 0))
  "</code></pre>")


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/pre-samp-kill "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/pre-samp-kill
  "Format a kill as a samp"
  nil
  "<pre class=\"samp\" xml:space=\"preserve\"><samp>"
  (xml-escape-string (current-kill 0))
  "</samp></pre>"
  )


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/var-contenteditable "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/var-contenteditable
  "Make a contenteditable var"
  nil
  "<var contenteditable=\"true\" spellcheck=\"false\">" p "</var>"
  )

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/var-contenteditable-from-kill "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/var-contenteditable-from-kill
  "Insert kill as content-editable var. Doesn't do escaping."
  nil
  "<var contenteditable=\"true\" spellcheck=\"false\">"
  (current-kill 0)
  "</var>"
  )


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/quote-code "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/quote-code
  "Put code in curly quotes"
  nil
  "“<code>" p "</code>”")


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/ahref "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/ahref
  "Simple a href link"
  nil
  "<a href=\"" p "\">" p "</a>")


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/1date "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/1date
  "Insert a <time> element with current date."
  nil
  "<time datetime=\"" (format-time-string "%Y-%m-%d") "\">" (format-time-string "%Y-%m-%d") "</time>")

(defvar jmm-unicode-superscript-table
  #s(hash-table size 10 test equal
		data ("1" "¹" "2" "²" "3" "³" "4" "⁴" "5" "⁵" "6" "⁶" "7" "⁷" "8" "⁸" "9" "⁹" "0" "⁰")))


(defvar jmm-unicode-subscript-table
  #s(hash-table size 10 test equal
		data ("1" "₁" "2" "₂" "3" "₃" "4" "₄" "5" "₅" "6" "₆" "7" "₇" "8" "₈" "9" "₉" "0" "₀")))

(defun jmm-get-unicode-superscript (char)
  (gethash char jmm-unicode-superscript-table char))

(defun jmm-translate-numbers-to-superscript (string)
  "Convert numbers in STRING to a unicode superscript"
  (replace-regexp-in-string "[0-9]" #'jmm-get-unicode-superscript string))

;; (jmm-translate-numbers-to-superscript "12390")

(defun jmm-get-unicode-subscript (char)
  (gethash char jmm-unicode-subscript-table char))

(defun jmm-translate-numbers-to-subscript (string)
  "Convert numbers in STRING to a unicode subscript"
  (replace-regexp-in-string "[0-9]" #'jmm-get-unicode-subscript string))

;; (jmm-translate-numbers-to-subscript "12390")

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/fraction-slash "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/fraction-slash
  "Fraction solidus and two numbers"
  nil
  (jmm-translate-numbers-to-superscript (read-string "Numerator: "))
  "⁄"
  (jmm-translate-numbers-to-subscript (read-string "Denominator: ")))


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/code-quick "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/code-quick
  "Quickly insert an inline code element"
  nil
  "<code>" (xml-escape-string (read-string "Code: ")) "</code>")


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/ahref-internal "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/ahref-internal
  "A href, but for internal pages"
  nil
  "<a class=\"internal\" href=\"" p "\">" p "</a>")

;;;###autoload (autoload 'a-kill-samepage "jmm-tempo")
(define-jmm-tempo a-kill-samepage
  "Make a link to the current kill, give it \"samepage\" class."
  nil
  (jte:set urlstr (current-kill 0))
  "<a href=\"" (xml-escape-string (jtu:get urlstr)) "\" class=\"samepage\">"
  p
  "</a>")




(defun jmm--kbd-element (s)
  (format "<kbd>%s</kbd>" (xml-escape-string s)))

;;;###autoload (autoload 'kbd-space-separated "jmm-tempo")
(define-jmm-tempo kbd-space-separated
  "Keyboard key sequences, space separated"
  nil
  (P "Key sequence (space separated): " keyseq t)
  "<kbd class=\"keycaps\">"
  (mapconcat #'jmm--kbd-element (string-split (jtu:get keyseq) " ") " ")
  "</kbd>"
  )

(defun jmm--kbd-plus-separate (str)
  (mapconcat #'jmm--kbd-element (string-split str "+") "+"))

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/kbd-plus-separated "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/kbd-plus-separated
  "Keyboard key caps, separated with plusses"
  nil
  (P "Key sequence (plus separated): " keyseq t)
  "<kbd class=\"keycaps\">"
  (mapconcat #'jmm--kbd-plus-separate (string-split (jtu:get keyseq) " ") " ")
  "</kbd>"

  )

(defun jmm--kbd-hyphen-separate (str)
  (mapconcat #'jmm--kbd-element (string-split str "-") "-"))

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/kbd-minus-separated "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/kbd-minus-separated
  "Keyboard key caps, separated with minuses/hyphens. Easier to type than plusses."
  nil
  (P "Key sequence (hyphen separated): " keyseq t)
  "<kbd class=\"keycaps\">"
  (mapconcat #'jmm--kbd-hyphen-separate (string-split (jtu:get keyseq) " ") " ")
  "</kbd>"
  )

;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/labeled-checkbox "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/labeled-checkbox
  "Make a <label> with an <input type=\"checkbox\"/> inside"
  nil
  "<label><input type=\"checkbox\"/> " p "</label>"
  )


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/redaction "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/redaction
  "Redact the current kill with light shade and a span."
  nil
  "<span class=\"redacted\">"
  (make-string (length (string-trim (current-kill 0))) ?░)
  "</span>"
  )


;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/a-external-link-kill-title-no-text "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/a-external-link-kill-title-no-text
  "Make a link from URL in current kill, fetching title and adding it as attribute. Text for link will be the URL directly. Classes are \"external\""
  nil
  (jte:set urlstr (current-kill 0))
  (jte:set title (or (jmm--get-url-html-title (jtu:get urlstr)) ""))
  "<a href=\"" (xml-escape-string (jtu:get urlstr)) "\" title=\"" (xml-escape-string (jtu:get title)) "\" class=\"external\" rel=\"external\" data-retrieved=\"" (format-time-string "%Y-%m-%d") "\">"
  (jte:set url (ignore-errors (url-generic-parse-url (jtu:get urlstr))))
  p
  (jte:bind (urlstr url title)
    (xml-escape-string urlstr))
  "</a>")




;;;###autoload (autoload 'jmm-tempo/jmm-xhtml/td1 "jmm-tempo")
(define-jmm-tempo jmm-tempo/jmm-xhtml/td1
  "Inline table cell"
  nil
  "<td>" p "</td>")



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

;;;###autoload (autoload 'jmm-tempo/css/fontify-kill-css "jmm-tempo")
(define-jmm-tempo jmm-tempo/css/fontify-kill-css
  "Add fontification styles for current kill"
  nil
  (cdr (jmm-htmlfontify-string (current-kill 0))))



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


;;;###autoload (autoload 'jmm-tempo/js2/arrow-function "jmm-tempo")
(define-jmm-tempo jmm-tempo/js2/arrow-function
  "Makes an arrow function, putting the cursor at the body"
  nil
  "(" p ") => {" n> 
  "" > p n
  "}" > 
  )


;;;###autoload (autoload 'jmm-tempo/js2/add-event-listener "jmm-tempo")
(define-jmm-tempo jmm-tempo/js2/add-event-listener
  "Template for addEventListener"
  nil
  "addEventListener(\"" p "\", " p ")")


;;;###autoload (autoload 'jmm-tempo/js2/set-attribute "jmm-tempo")
(define-jmm-tempo jmm-tempo/js2/set-attribute
  "Template for setAttribute"
  nil
  "setAttribute(\"" p "\", " p ")"
  )


;;;###autoload (autoload 'jmm-tempo/js2/document-createelement "jmm-tempo")
(define-jmm-tempo jmm-tempo/js2/document-createelement
  "Template for document.createElement"
  nil
  "document.createElement(\"" (P "Element type: " name) "\")"
  )



;;;;;;;;;;
;; (timebox-xml-mode-abbrev-table)

;;;###autoload (autoload 'jmm-tempo/timebox-xml/ttodo "jmm-tempo")
(define-jmm-tempo jmm-tempo/timebox-xml/ttodo
  "Make a <todo> element"
  nil
  (P "Title: " title t)
  (jte:set id (jmm-skeleton-prompt "ID: " (list (jmm-tempo--string-to-xml-id (jtu:get title))
					      (jmm-xhtml--gen-new-id))))
  "<todo"
  " created=\"" (xml-escape-string (format-time-string "%Y-%m-%d %H:%M")) "\""
  (jte:when-some id
    " id=\"" (xml-escape-string id) "\"")
  ">" n>
  "<summary>" (jt:if-some title (s title) p) "</summary>" n>
  "<description>" n>
  p > n
  "</description>" > n>
  "</todo>" >
)

;;;###autoload (autoload 'jmm-tempo/timebox-xml/workk "jmm-tempo")
(define-jmm-tempo jmm-tempo/timebox-xml/workk
  "Add <work> tag using ID in current kill."
  nil
  (jt:set id (xml-escape-string (current-kill 0)))
  (jt:set title (xml-escape-string (timebox-xml--get-title-for-todo-id (jtu:get id))))
  (jt:if-some title
      (l "<work on=\"" (s id) "\" title=\"" (s title) "\">" p "</work>")
    (l "<work on=\"" (s id) "\"/> <!-- " p " -->")))


;;;###autoload (autoload 'jmm-tempo/timebox-xml/depends-k "jmm-tempo")
(define-jmm-tempo jmm-tempo/timebox-xml/depends-k
  "Add dependency on current ID in kill"
  nil
  (jt:set id (current-kill 0))
  (jt:set title (timebox-xml--get-title-for-todo-id (jtu:get id)))
  "<dependency on=\"" (xml-escape-string (jtu:get id)) "\" title=\"" (xml-escape-string (jtu:get title)) "\"/>" > )


;;;###autoload (autoload 'jmm-tempo/timebox-xml/goalk "jmm-tempo")
(define-jmm-tempo jmm-tempo/timebox-xml/goalk
  "Add a goal using ID of current kill"
  nil
  (jt:set id (current-kill 0))
  (jt:set title (timebox-xml--get-title-for-todo-id (jtu:get id)))
  "<goal todo=\"" (xml-escape-string (jtu:get id)) "\" title=\"" (xml-escape-string (jtu:get title)) "\"/>" > )

;;;###autoload (autoload 'jmm-tempo/timebox-xml/sstat "jmm-tempo")
(define-jmm-tempo jmm-tempo/timebox-xml/sstat
  "Update status of a todo"
  nil
  (jte:set state (completing-read "State: " '("todo" "done" "cancelled" "waiting")))
  (ignore (save-excursion (timebox-xml-update-todo-state (jtu:get state))))
  ;; TODO: Also set parent state.
  "<status"
  (l
    " state=\"" (s state) "\"")
  " timestamp=\"" (format-time-string "%FT%T%z") "\">" n> 
  p n> 
  "</status>" > )



;;;;;;;;;;
;; (eshell-mode-abbrev-table)

;;;###autoload (autoload 'jmm-tempo/eshell/ffrecord "jmm-tempo")
(define-jmm-tempo jmm-tempo/eshell/ffrecord
  "A template for recording with ffmpeg"
  nil
  "ffmpeg -fflags nobuffer -flags low_delay "
  "-f pulse -i " (jmm-read-pulseaudio 'source "Left side") " "
  "-f pulse -i " (jmm-read-pulseaudio 'source "Right side") " "
  "-filter_complex \"[0:a][1:a]amerge=inputs=2,pan=stereo|c0<c0+c1|c1<c2+c3[a]\" -map \"[a]\" "
  "-fflags +genpts " (P "Filename: " filename))


;;;###autoload (autoload 'jmm-tempo/eshell/ffmeta "jmm-tempo")
(define-jmm-tempo jmm-tempo/eshell/ffmeta
  "Add metadata to a file"
  nil
  (jte:set file (file-relative-name (expand-file-name (read-file-name "File: "))))
  "ffmpeg -i " (shell-quote-argument (jtu:get file))
  (jte:when-set (title (read-string "Title: "))
    " -metadata title=\"" (s title) "\"")
  (jte:while-set (field (read-string "Field: "))
     " -metadata " (s field) "=\"" (read-string "Value: ") "\"")
  " -c copy "
  (shell-quote-argument (file-name-sans-extension (jtu:get file)))
  p "." (file-name-extension (jtu:get file)))



;;;;;;;;;;
;; (org-mode-abbrev-table)

(defun jmm-tempo--random-base58 ()
  (let* ((alnum "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz")
         (i (random (length alnum))))
    (substring alnum i (1+ i))))

(defun jmm-tempo--jmm-org-random-id (title)
  "A random org CUSTOM_ID, given a title."
  (format "%s-%s-%s%s%s%s"
	  (jmm-tempo--string-to-xml-id title)
	  (format-time-string "%Y%02m%02d")
	  (jmm-random-base58) (jmm-random-base58)
	  (jmm-random-base58) (jmm-random-base58)))

;;;###autoload (autoload 'jmm-tempo/org/ttodo "jmm-tempo")
(define-jmm-tempo jmm-tempo/org/ttodo
  "A simple new todo"
  nil
  (P "Title: " title t)
  (jte:set id (jmm-tempo--jmm-org-random-id (jtu:get title)))
  "** TODO " (s title) p "
:PROPERTIES:
:CREATED:  " (format-time-string "[%Y-%02m-%02d %a %H:%M]") "
:ID:       " (string-trim-right (shell-command-to-string "uuidgen")) "
:END:
" p
)

(defun jmm-tempo--org-inactive-timestamp ()
  "A simple inactive timestamp for current time."
  (format-time-string "[%Y-%02m-%02d %a %H:%M]"))

(defun jmm-tempo--org-maybe-set-property (property value)
  "Set PROPERTY, if it does not exist, to VALUE."
  (unless (org-entry-get (point) property)
     (save-excursion
       (org-entry-put (point) property value))))

;;;###autoload (autoload 'jmm-tempo/org/crtd "jmm-tempo")
(define-jmm-tempo jmm-tempo/org/crtd
  "Set CREATED property if it doesn't exist."
  nil
  (ignore
   (jmm-tempo--org-maybe-set-property "CREATED" (jmm-tempo--org-inactive-timestamp))
   (jmm-tempo--org-maybe-set-property "ID" (string-trim-right (shell-command-to-string "uuidgen")))
   ))

(defun jmm-tempo--org-link (linkinfo)
  "Take LINKINFO from popping `org-stored-links', return text of org link."
  (format "[[%s][%s]]" (car linkinfo) (cadr linkinfo)))

(defun jmm-tempo--org-link-tag-notmuch (linkinfo tags)
  "Add tags to notmuch email.
LINKINFO is like from `org-stored-links'.
TAGS is a list like (list \"+inorg\" \"+todo\")"
  (save-window-excursion
    (org-link-open-from-string (car linkinfo))
    (notmuch-show-tag tags))
  nil)

;;;###autoload (autoload 'jmm-tempo/org/temail "jmm-tempo")
(define-jmm-tempo jmm-tempo/org/temail
  "Make a todo for an email"
  nil
  ;; TODO: Should we pop or just leave it?
  (jte:set link (pop org-stored-links))
  (jte:set etitle (cadr (jtu:get link)))
  (jte:set title (jmm-skeleton-prompt "Title: " (list (jtu:get etitle))))

  "** TODO " p  (s title) " :email:toread:" p "
:PROPERTIES:
:ID:       " (string-trim-right (shell-command-to-string "uuidgen")) "
:CREATED:  " (jmm-tempo--org-inactive-timestamp) "
:END:
" (jmm-tempo--org-link (jtu:get link)) "

"
  (jmm-tempo--org-link-tag-notmuch (jtu:get link) (list "+inorg" "+todo"))
)

;; Maybe for reading tags in the future.
;; (let ((crm-separator " "))
;;   (completing-read-multiple
;;    "Tags: "
;;    (apply-partially
;;     #'completion-table-with-terminator " "
;;     (org-global-tags-completion-table
;;      (org-agenda-files)))))


;;;###autoload (autoload 'jmm-tempo/org/oemail "jmm-tempo")
(define-jmm-tempo jmm-tempo/org/oemail
  "Insert link to email, mark email as inorg"
  nil
  (jte:set link (car org-stored-links))
  (jmm-tempo--org-link (jtu:get link))
  (jmm-tempo--org-link-tag-notmuch (jtu:get link) (list "+inorg"))
  )

;;;###autoload (autoload 'jmm-tempo/org/grocery "jmm-tempo")
(define-jmm-tempo jmm-tempo/org/grocery
  "Part of a template for capturing groceries within `org-capture-templates'.
Trying to do this as a hook."
  nil
  (ignore
   (move-end-of-line 1) ;; Org capture starts with the point at the beginning.
   (just-one-space))
  (P "Grocery item: " title t)
  (jte:set id (jmm-tempo--jmm-org-random-id (jtu:get title)))
  "TODO " (s title) p " :SHOPPING:grocery:" p "
:PROPERTIES:
:CREATED:  " (format-time-string "[%Y-%02m-%02d %a %H:%M]") "
:ID:       " (string-trim-right (shell-command-to-string "uuidgen")) "
:END:
" p
)


(provide 'jmm-tempo)
;;; jmm-tempo.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("jt-" . "jmm-tempo-") ("jt:" . "jmm-tempo-element:") ("jte:" . "jmm-tempo-element:") ("jtu:" . "jmm-tempo-util:"))
;; End:
