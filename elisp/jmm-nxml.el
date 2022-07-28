;;; jmm-nxml.el --- Josh's utilities for nxml-mode   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Joshua Moller-Mara

;; Author: Joshua Moller-Mara <jmm@cns.nyu.edu>
;; Keywords: wp, hypermedia, languages, XML

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

;; These are utilities to extend nXML-mode.
;; For example there are utilities to wrap regions in a tag, change attribute values, and so on.

;; To-dos:
;; - [X] A function to convert between block and inline contents
;; - [ ] Maybe add XML namespaces to child tags

;;; Code:

(require 'nxml-mode)

;;; Variables:

(defvar-keymap jnx-minibuffer-local-completion-with-spaces-map
  :doc "Same as `minibuffer-local-completion-map', but allows spaces.
Use like:
(let ((jmm/nxml-minibuffer-local-completion-map minibuffer-local-completion-with-spaces-map))
  (completing-read PROMPT COMPLETIONS)
  )"
  :parent minibuffer-local-completion-map
  "SPC" nil)

(defvar jnx-element-history nil
  "History for things like `jmm/nxml-swap'")

(defvar jnx-class-history nil
  "History for `jmm/nxml-prompt-class'")



;;; Internal utilities:

(defun jnx--string-or-nil (str)
  "Return nil if string is blank, otherwise return STR."
  (if (string-empty-p str)
      nil
    str))

(defun jnx--element-for-point ()
  "Scan the xmltok for the element at or around point.
It's the parent element, or current element if looking at the start of a tag."
  (unless (and (looking-at-p "<")
	       (nxml-token-after)
	       (memq xmltok-type '(start-tag empty-element)))
    (nxml-backward-up-element)))

(defmacro jnx-at-element-start (&rest body)
  "Run body with point and xmltok set to `jmm/nxml--element-for-point'."
  (declare (indent 0) (debug t))
  `(save-excursion
     (jnx--element-for-point)
     ,@body))

;; Should this be an `unwind-protect'?
(defmacro jnx-let-markers* (binders &rest body)
  "Like `let*', but each marker is freed afterward."
  (declare (indent 1) (debug let))
  `(let* ,binders
     (prog1
	 (progn ,@body)
       ,@(mapcar (lambda (binder)
		   `(set-marker ,(if (consp binder) (car binder) binder) nil))
		 binders))))

(defun jnx--delete-blank-line ()
  "Delete the current line if it's blank.
Like `delete-blank-lines', but only for one line."
  (save-excursion
  (beginning-of-line)
  (when (looking-at "[ \t]*$")
    (delete-region (line-beginning-position) (progn (forward-line 1) (point))))))


;;; Navigating between tags

(defun jnx--move-while (fun)
  "Keep running FUN until we no longer change point."
  (while (let ((pos (point))
	       (pos2 (progn (ignore-errors (funcall fun))
			    (point))))
	   (/= pos pos2))
    nil))

(defun jnx--next-tag ()
  "Go to next start-tag or empty-element."
  (catch 'found
    (while (xmltok-forward)
      (when (memq xmltok-type '(start-tag empty-element))
	(throw 'found t)))))

(defun jnx--next-tag-same-level ()
  "Find next start tag (or empty element) at same level."
  (let ((end (nxml-token-after))
	start lasttype)
    (when (memq xmltok-type '(start-tag empty-element))
      (nxml-forward-single-balanced-item)
      (setq end (nxml-token-after)))
    (while (not (memq xmltok-type '(end-tag partial-end-tag start-tag empty-element)))
      (when (eq xmltok-type 'space)
	(setq start (point)))
      (setq lasttype xmltok-type)
      (goto-char end)
      (setq end (nxml-token-after)))
    (if (memq xmltok-type '(start-tag empty-element))
	end
      (progn
	(if (eq lasttype 'space)
	    (goto-char start))
	nil))))

(defun jnx--next-tagname (tagname-or-pred)
  "Find next tag with string tagname.
TAGNAME-OR-PRED can also be a predicate of no arguments that inspects xmltok.
If TAGNAME-OR-PRED is nil, just runs `jmm/nxml--next-tag'"
  (cond
   ((null tagname-or-pred) (jnx--next-tag))
   ((stringp tagname-or-pred)
    (catch 'found
      (while (jnx--next-tag)
	(when (string= (xmltok-start-tag-qname) tagname-or-pred)
	  (throw 'found t)))))
   (t
    (catch 'found
      (while (jnx--next-tag)
	(when (funcall tagname-or-pred)
	  (throw 'found t)))))))

(defun jnx--ancestor-tagname (tagname-or-pred)
  "Find ancestor tag with string tagname.
TAGNAME-OR-PRED can also be a predicate of no arguments that inspects xmltok.
If TAGNAME-OR-PRED is nil, tries to go up one level.
Returns t if something was found."
  (cond
   ((null tagname-or-pred)
    (let ((start (point)))
      (nxml-backward-up-element)
      (/= start (point))))
   ((stringp tagname-or-pred)
    (catch 'found
      (jnx--move-while
       (lambda ()
	 (nxml-backward-up-element)
	 (when (string-equal (xmltok-start-tag-qname) tagname-or-pred)
	   (throw 'found t))))))
   (t
    (catch 'found
      (jnx--move-while
       (lambda ()
	 (nxml-backward-up-element)
	 (when (funcall tagname-or-pred)
	   (throw 'found t))))))))

(defun jnx--next-tagname-same-level (tagname-or-pred)
  "Find tagname-or-pred at same level."
  (cond
   ((stringp tagname-or-pred)
    (catch 'found
      (while (jnx--next-tag-same-level)
	(when (string= (xmltok-start-tag-qname) tagname-or-pred)
	  (throw 'found (point))))))
   (t
    (catch 'found
      (while (jnx--next-tag-same-level)
	(when (funcall tagname-or-pred)
	  (throw 'found (point))))))))

(defun jnx--last-tagname-same-level (tagname)
  "Find last tagname at the same level."
  (let (last)
    (save-excursion
      (while (jnx--next-tagname-same-level tagname)
	(setq last (point))))
    (when last
      (goto-char last))))

(defun jnx--find-or-add-tagname-same-level (tagname)
  "Find tagname at same level. Otherwise add it to the end."
  (or (jnx--next-tagname-same-level tagname)
      (progn
	(newline-and-indent)
	(save-excursion (jnx-add-element tagname nil "\n"))
	xmltok-start)))



;;; Completions

(defmacro jnx-dynamic-completion (&rest body)
  "Runs body from current buf at same point.
Returns a `completion-table-dynamic'.
Caches the body, so it only runs once."
  (declare (indent 0) (debug t))
  (let ((buf (make-symbol "buf"))
	(pos (make-symbol "pos"))
	(last-res (make-symbol "last-res")))
    `(let ((,buf (current-buffer))
	   (,pos (point))
	   ,last-res)
       (completion-table-with-cache
	(lambda (_)
	  (or ,last-res
	      (setq ,last-res
		    (with-current-buffer ,buf
		      (save-excursion
			(goto-char ,pos)
			,@body)))))
	t))))

(defun jnx--completions-for-attribute-names (tagname-or-pred &optional extras)
  "Return a dynamic completion table of all attribute names.
Set TAGNAME-OR-PRED to nil to return all attribute names.
Add EXTRAS to the table."
  (jnx-dynamic-completion
    (goto-char (point-min))
    (let ((tags (make-hash-table :test 'equal)))
      (cl-loop for x in extras
	       do (puthash x t tags))
      (while (jnx--next-tagname tagname-or-pred)
	(cl-loop for x in (jnx--attribute-names)
		 do (puthash x t tags)))
      tags)))

(defun jnx--completions-for-attribute-values (tagname-or-pred attrname &optional extras)
  "Return a dynamic completion table for all attribute values for ATTRNAME for tag TAGNAME-OR-PRED.
See `jmm/nxml--next-tagname' for TAGNAME-OR-PRED.
Add EXTRAS to the table."
  (jnx-dynamic-completion
    (append extras
	    (jnx--get-all-attribute-values tagname-or-pred attrname))))

(defun jnx--completions-for-attribute-values-space-separated (tagname-or-pred attrname &optional extras)
  "Return a dynamic completion table for all space-separated attribute values for ATTRNAME for tag TAGNAME-OR-PRED.
Usually used to return a list of classes.
See `jmm/nxml--next-tagname' for TAGNAME-OR-PRED.
Add EXTRAS to the table."
  (jnx-dynamic-completion
    (append extras
	    (jnx--get-all-attribute-values-space-separated tagname-or-pred attrname))))

(defun jnx--completions-for-tagnames (&optional extras)
  "Return a dynamic completion table of all qnames.
Add EXTRAS to the table."
  (jnx-dynamic-completion
    (goto-char (point-min))
    (let ((tags (make-hash-table :test 'equal)))
      (cl-loop for x in extras
	       do (puthash x t tags))
      (while (jnx--next-tag)
	(puthash (xmltok-start-tag-qname) t tags))
      tags)))

(defun jnx-prompt-tagname (prompt &optional default)
  "Prompt for an element name (possibly with attrs).
Allows you to enter in spaces.
Takes in an optional default DEFAULT."
  (let ((minibuffer-local-completion-map jnx-minibuffer-local-completion-with-spaces-map))
    (completing-read (format-prompt prompt default)
		     (jnx--completions-for-tagnames jmm/nxml-element-history)
		     nil nil nil
		     'jmm/nxml-element-history
		     default)))

;; TODO: Make completion work for multiple values
(defun jnx-prompt-class (prompt &optional initial)
  "Prompt for a class name (or space-separated class names).
Allows you to enter in spaces."
  (let ((minibuffer-local-completion-map jnx-minibuffer-local-completion-with-spaces-map))
    (completing-read (format-prompt prompt nil)
		     (jnx--completions-for-attribute-values-space-separated nil "class" jmm/nxml-class-history)
		     nil nil
		     initial
		     'jmm/nxml-class-history)))


;;; Moving and modifying the nXML buffer

(defun jnx--element-bounds ()
  "Returns a cons of the `xmltok-start' and position of matching end tag."
  (let ((start xmltok-start))
    (cons
     start
     (save-excursion
       (xmltok-save
	 (goto-char start)
	 (nxml-forward-single-balanced-item)
	 (point))))))

(defun jnx-beginning-of-inner-sexp ()
  "Go to the first element of sexp.
Doesn't move outside current level."
  (interactive nil nxml-mode)
  (jnx--move-while #'nxml-backward-single-balanced-item))

(defun jnx-end-of-inner-sexp ()
  "Go to the last element of sexp.
Doesn't move outside current level.
Note that this doesn't go to the point before the enclosing end-tag,
It'll go to the point after the last inner end-tag."
  (interactive nil nxml-mode)
  (jnx--move-while #'nxml-forward-single-balanced-item))

;; Modifying elements
(defun jnx-swap (newelem)
  "Tries to change the qname of surrounding element.
NEWELEM is a string, which can also have attribute values."
  (interactive (list (jnx-prompt-tagname (format "Change \"%s\" to"
						 (jnx-at-element-start
						   (xmltok-start-tag-qname)))))
	       nxml-mode)
  (jnx-at-element-start
    (let* ((origtype xmltok-type)
	   qname)
      (jnx-let-markers* ((s1 (save-excursion
			       (goto-char (1+ xmltok-start))
			       (point-marker)))
			 (e1 (save-excursion
			       (goto-char xmltok-name-end)
			       (point-marker)))
			 (e2 (save-excursion (nxml-forward-single-balanced-item)
					     (goto-char (+ 2 xmltok-start))
					     (point-marker)))
			 (s2 (save-excursion
			       (goto-char xmltok-name-end)
			       (point-marker))))
	(atomic-change-group
	  (goto-char s1)
	  (delete-region s1 e1)
	  (insert newelem)
	  (nxml-backward-up-element)
	  ;; Don't swap for empty-element
	  (when (eq origtype 'start-tag)
	    (setq qname (xmltok-start-tag-qname))
	    (goto-char s2)
	    (delete-region s2 e2)
	    (insert qname)))))))


(defun jnx-wrap (elemstr &optional block beg end)
  "Tries to wrap region with element.
With BLOCK, it'll add a newline and indent the region..
If region isn't active, wrap the point.
Returns the bounds of what's been inserted."
  (interactive (let* ((block current-prefix-arg)
		      (mystr (jmm/nxml-prompt-element
			      (format "%s element with attrs"
				      (if block "Block" "Inline")))))
		 (if (use-region-p)
		     (list mystr block (region-beginning) (region-end))
		   (list mystr block nil nil)))
	       nxml-mode)
  (let ((beg (or beg (point)))
	(end (or end (point)))
	(instr (format "<%s>" elemstr))
	qname)
    (jnx-let-markers* ((s1 (progn (goto-char beg)
				  (point-marker)))
		       (e1 (save-excursion
			     (goto-char end)
			     (point-marker)))
		       m1)
      (set-marker-insertion-type e1 t)
      (atomic-change-group
	(goto-char s1)
	(insert-before-markers instr)
	;; Inserting before messes up s1, so we need to adjust it.
	(save-excursion
	  (backward-char (length instr))
	  (set-marker s1 (point)))
	(setq qname (progn (nxml-token-before) (xmltok-start-tag-qname)))
	(when block (newline))
	(setq m1 (point-marker))
	(unless (= beg end)
	  (set-marker-insertion-type m1 t))
	(goto-char e1)
	(when block (newline))
	(insert (format "</%s>" qname))
	;; (nxml-finish-element)
	(when block
	  ;; TODO: Messages still displayed. Something else for
	  ;; indenting the region without prompting?
	  (let ((inhibit-message t))
	    (indent-region s1 e1)))
	(goto-char s1)
	;; Ensure the last scanned element is the element we just
	;; created.
	(nxml-token-after)
	(goto-char m1)
	(cons (marker-position s1)
	      (marker-position e1))))))

(defun jnx-unwrap ()
  "Delete the surrounding element, keeping children."
  (interactive nil nxml-mode)
  (save-excursion
    (nxml-backward-up-element)
    (jnx-let-markers* ((s1 (point-marker))
		       (e1 (save-excursion
			     (xmltok-forward)
			     (point-marker)))
		       (e2 (progn (nxml-forward-single-balanced-item)
				  (point-marker)))
		       (s2 (save-excursion
			     (nxml-token-before)
			     (goto-char xmltok-start)
			     (point-marker))))
      (atomic-change-group
	(goto-char s1)
	(delete-region s1 e1)
	(jnx--delete-blank-line)
	(goto-char s2)
	(delete-region s2 e2)
	(jnx--delete-blank-line)
	(let ((inhibit-message t))
	  (indent-region s1 e2))
	(cons (marker-position s1)
	      (marker-position e2))))))

;; MAYBE: Convert empty-element?
(defun jnx-blockify-element ()
  "Ensure the current element is a block element, not inline.
Should be idempotent.
Last scanned element will be the start-tag of the blockified element.
Returns boundaries of element from `jmm/nxml--element-bounds'. "
  (interactive nil nxml-mode)
  (jnx-let-markers* ((a1 (point-marker))
		     (s1 (progn
			   (jnx--element-for-point)
			   (point-marker)))
		     (e1 (save-excursion
			   (xmltok-forward)
			   (point-marker)))
		     (e2 (progn (nxml-forward-single-balanced-item)
				(point-marker)))
		     (s2 (save-excursion
			   (goto-char xmltok-start)
			   (point-marker))))
    (let ((was-empty (= e1 (1- s2))))
      (atomic-change-group
	(when (= a1 s2)
	  (set-marker-insertion-type a1 nil))
	(goto-char s2)
	(unless (= (point) (save-excursion (back-to-indentation) (point)))
	  (insert "\n")
	  (nxml-indent-line))
	(when (= a1 e1)
	  (set-marker-insertion-type a1 t))
	(goto-char e1)
	(when (or was-empty
		  (not (eolp)))
	  (insert "\n")
	  (nxml-indent-line))
	;; Should we indent the entire block?
	;; (let ((inhibit-message t))
	;;   (indent-region s1 e2)
	;; Ensure last scanned item is the element we're blockifying
	(goto-char s1)
	(nxml-token-after)
	(prog1
	    (jnx--element-bounds)
	    (goto-char a1))))))

(defun jnx-inline-element ()
  "Ensure the current element is inline, not a block element.
Folds all elements onto one line.
Tries to maintain spaces between words and double spaces between
  sentences if `sentence-end-double-space' is set.
Should be idempotent.
Last scanned element will be the start-tag of the blockified element.
Returns boundaries of element from `jmm/nxml--element-bounds'."
  (interactive nil nxml-mode)
  (jnx-let-markers* ((a1 (point-marker))
		     (s1 (progn
			   (jnx--element-for-point)
			   (point-marker)))
		     (s2 (progn (nxml-forward-single-balanced-item)
				(goto-char xmltok-start)
				(point-marker))))
    (let ((sentence-end-regex (sentence-end))
	  (no-space-join '(start-tag end-tag empty-element))
	  at-sentence)
      (atomic-change-group
	(goto-char s2)
	(while (> (progn (forward-line 0) (point)) s1)
	  (back-to-indentation)
	  (when sentence-end-double-space
	    ;; Check if the previous line end is the end of a sentence.
	    (save-excursion
	      (forward-line -1)
	      (end-of-line)
	      ;; The line might end with whitespace
	      (skip-syntax-backward " " (line-beginning-position))
	      (setq at-sentence
		    (unless (bolp)
		      (backward-char)
		      (looking-at-p sentence-end-regex)))))
	  (delete-horizontal-space t)
	  (if (= (preceding-char) ?\n)
	      (delete-char -1)
	    (error "I didn't work out the logic correctly. Rewrite this function."))
	  ;; If we're joining at text, we need to add whitespace back in.
	  (when (and (not (memq (progn (nxml-token-after) xmltok-type) no-space-join))
		     (not (memq (progn (nxml-token-before) xmltok-type) no-space-join)))
	    (fixup-whitespace)
	    ;;  Use two spaces if we're at the end of a sentence, and
	    ;;  `sentence-end-double-space' is set.
	    (when at-sentence
	      (insert ?\s))))
	;; Ensure last scanned item is the element we're blockifying
	(goto-char s1)
	(nxml-token-after)
	(prog1
	    (jnx--element-bounds)
	  (goto-char a1))))))

(defun jnx-add-element (tagname attrs &rest children)
  "Insert a new element at point.
TAGNAME is a string.
ATTRS is an alist of attributes.
Children is some children, like used in `xml-debug-print'."
  (let ((start (point))
	(end nil)
	(qname (if (symbolp tagname) tagname
		 (intern tagname)))
	(attrs2 (seq-remove (lambda (x) (null (cdr x))) attrs)))
    (xml-debug-print (list `(,qname ,attrs2 ,@children)))
    (setq end (point))
    (indent-region start end)
    (save-excursion
      (goto-char start)
      (nxml-token-after))))


;;; Attributes

;; DONE: Could prematurely optimize this by not calling
;; `xml-substitute-special' if no character entities are present.
;; Can look at `xmltok-attribute-refs'.
(defun jnx--xmltok-attribute-unescaped-value (attr)
  "Take in an ATTR from `xmltok-attributes', return its unescaped value."
  (let ((str (buffer-substring-no-properties
	      (xmltok-attribute-value-start attr)
	      (xmltok-attribute-value-end attr))))
    (if (xmltok-attribute-refs attr)
	(xml-substitute-special str)
      str)))

(defun jnx--attribute-value (attrname)
  "Return the unescaped attribute value for ATTRNAME, as a string.
This assumes you've already have `xmltok-attributes' scanned."
  (catch 'attrval
    (cl-loop for attr in xmltok-attributes
	     do (when (string= attrname (xmltok-attribute-local-name attr))
		  (throw 'attrval (jnx--xmltok-attribute-unescaped-value attr))))))

(defun jnx--attribute-names ()
  "Return list of attribute names.
This assumes you already have `xmltok-attributes' scanned."
  (cl-loop for attr in xmltok-attributes
	   collect (xmltok-attribute-local-name attr)))

(defun jnx--attribute-delete (attr)
  "Delete xmltok ATTR."
  (let ((beg (1- (xmltok-attribute-name-start attr)))
	(end (1+ (xmltok-attribute-value-end attr))))
    (save-excursion
      (goto-char beg)
      (delete-region beg end))))

(defun jnx--attribute-replace (attr str)
  "Set xmltok ATTR to be escaped value STR.
If str is nil, delete the attribute."
  (if str
      (let ((beg (xmltok-attribute-value-start attr))
	    (end (xmltok-attribute-value-end attr)))
	(save-excursion
	  (goto-char beg)
	  (delete-region beg end)
	  (insert (xml-escape-string str))))
    (jnx--attribute-delete attr)))

(defun jnx--attribute-insert (attrname attrval)
  "Insert an XML attribute at point.
See `xml-debug-print-internal'."
  (when attrval ;; Don't insert anything if attrval is nil.
    (insert ?\  attrname "=\""
	    (xml-escape-string attrval) ?\")))

(defun jnx--last-attribute-pos ()
  "Go to the point of the last attribute position."
  (if-let* ((lastattr (car (last xmltok-attributes))))
      (goto-char (1+ (xmltok-attribute-value-end lastattr)))
    (goto-char xmltok-name-end)))

(defun jnx--set-attribute-value (attrname attrval &optional append)
  "Sets attribute.
This assumes you've already scanned and set xmltok-attributes.
Will rescan after setting."
  (with-buffer-unmodified-if-unchanged
    (save-excursion
      (let* ((origstart xmltok-start)
	     foundattr)
	(prog1
	    (if (catch 'attrval
		  (cl-loop for attr in xmltok-attributes
			   do (when (string= attrname (xmltok-attribute-local-name attr))
				(setq foundattr attr)
				(throw 'attrval t))))
		(jnx--attribute-replace foundattr attrval)
	      ;; None found
	      (progn
		(if append
		    (jnx--last-attribute-pos)
		  (goto-char xmltok-name-end))
		(jnx--attribute-insert attrname attrval)))
	  (when origstart
	    (nxml-scan-element-forward origstart)))))))

;; A fun way to use `jnx--attribute-value' for `setf'
(gv-define-setter jnx--attribute-value (val attrname)
  `(progn
     (jnx--set-attribute-value ,attrname ,val t)
     ,val))

(defun jnx--update-attribute-value (attrname fun &optional append &rest funargs)
  "Update the attribute ATTRNAME.
Fun takes in the previous value and adds a new value.
Assumes you've already scanned."
  (let* ((prevval (jnx--attribute-value attrname))
	 (newval (xmltok-save (apply fun prevval funargs))))
    (jnx--set-attribute-value attrname newval append)))

(defun jnx--get-all-attribute-values (tagname-or-pred attrname)
  "Get all attribute values that appear in TAGNAME-OR-PRED.
See `jmm/nxml--next-tagname'.
TAGNAME-OR-PRED can be nil to get the attribute value anywhere."
  (let (values)
    (save-excursion
      (goto-char (point-min))
      (while (jnx--next-tagname tagname-or-pred)
	(when-let* ((val (jnx--attribute-value attrname)))
	  (push val values))))
    (delete-dups values)))

(defun jnx--get-all-attribute-values-space-separated (tagname-or-pred attrname)
  "Get all attribute values (separated with spaces) that appear in TAGNAME-OR-PRED.
Can be used to get a list of all classes, for example.
See `jmm/nxml--next-tagname'.
TAGNAME-OR-PRED can be nil to get the attribute value anywhere."
  (let (values)
    (save-excursion
      (goto-char (point-min))
      (while (jnx--next-tagname tagname-or-pred)
	(when-let* ((val (jnx--attribute-value attrname))
		    (uvals (split-string val)))
	  (cl-loop for x in uvals
		   do (push x values)))))
    (delete-dups values)))

(defun jnx-edit-attribute (attrname attrval &optional append)
  "Edit attribute for tag at or around point.
With APPEND, add new attributes to the end."
  (interactive
   (let* ((attrname (completing-read "Attribute name: "
				     ;; Attributes for specific tag.
				     ;; (jnx-dynamic-completion
				     ;;   (save-mark-and-excursion
				     ;;     (jnx--element-for-point)
				     ;;     (jnx--attribute-names)))
				     (jnx--completions-for-attribute-names nil)))
	  (attrval (jnx--string-or-nil
		    (let ((minibuffer-local-completion-map minibuffer-local-completion-with-spaces-map))
		      (completing-read "Attribute value: "
				       (jnx--completions-for-attribute-values nil attrname)
				       nil
				       nil
				       (jnx-at-element-start
					 (jnx--attribute-value attrname)))))))
     (list attrname attrval current-prefix-arg))
   nxml-mode)
  (jnx-at-element-start
    (jnx--set-attribute-value attrname attrval append)))


;; TODO: This actually isn't great since not all of the buffer will be fontified if we run "flyspell-buffer"
(defun jnx--flyspell-verify ()
  "See if point is a word worth spell checking."
  (let ((allowable '(nxml-text))
	(face (get-text-property (point) 'face)))
    (if (listp face)
	(seq-intersection face allowable)
      (memq face allowable))))

(provide 'jmm-nxml)
;;; jmm-nxml.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("jnx-" . "jmm/nxml-"))
;; End:
