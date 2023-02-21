;;; jmm-htmlfontify.el --- HTML fontification without <pre>   -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Joshua Moller-Mara

;; Author: Joshua Moller-Mara <jmm@cns.nyu.edu>
;; Keywords: html, hypermedia, markup

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

;; This does stuff similar to `htmlfontify-buffer', but doesn't
;; require the resulting output to be put inside a "<pre>" tag.
;; That is, it also escapes newlines and spaces.
;; 
;; The only reason I'm doing this is because I haven't figured out how
;; to get `nxml-mode' to skip indentation of "<pre>" forms in a good
;; way.
;;
;; The main function here is `jmm-htmlfontify-buffer'.
;;
;; `jmm-htmlfontify-css-buffer' tries to make a simplified version of
;; the CSS, but it's still pretty verbose.

;;; Code:

(require 'htmlfontify)
(require 'rx)

;; Things we escape
(rx-define jhfy-startspaces (seq bol (1+ " ")))
(rx-define jhfy-middlespaces (seq " " (1+ " ")))
(rx-define jhfy-starttabs (seq bol (1+ "\t")))
(rx-define jhfy-middletabs (seq (1+ "\t")))

;; Note: &nbsp; doesn't parse in XML.
(defun jhfy-start-spaces-quote (str _)
  (concat "&#xA0;" (make-string (1- (length str)) ? )))

(defun jhfy-start-tabs-quote (str pos)
  (save-excursion
    (let* ((start (progn (goto-char pos)
			 (current-column)))
	   (end (progn (goto-char (match-end 0))
		       (current-column))))
      (concat "&#xA0;" (make-string (1- (- end start)) ? )))))

(defun jhfy-middle-tabs-quote (str pos)
  (save-excursion
    (let* ((start (progn (goto-char pos)
			 (current-column)))
	   (end (progn (goto-char (match-end 0))
		       (current-column))))
      (make-string (- end start) ? ))))
    
    

(defun jhfy-middle-spaces-quote (str _)
  (make-string (length str) ? ))
;; end things we escape

(defvar jhfy-html-quote-map
  '((jhfy-startspaces jhfy-start-spaces-quote)
    (jhfy-starttabs jhfy-start-tabs-quote)
    (jhfy-middlespaces jhfy-middle-spaces-quote)
    (jhfy-middletabs jhfy-middle-tabs-quote)
    ("\n" "<br/>\n")
    ("\"" "&quot;")
    ("<" "&lt;")
    ("&" "&amp;")
    (">" "&gt;"))
  "Things to replace for our HTML.
An alist of (regex replacement).
Regex is a form that can be recognized by `rx-to-string'.
Replacement is either a string, or a function that takes in two arguments (the match, and the point of the match) and returns a string.
Alist is iterated in order, and only the first match is used to escape a property.
See also `hfy-html-quote-map'.")

(defvar jhfy-ignore-styles
  '(("text-decoration" "none")
    ("font-weight" "nil")
    ("font-style" "normal")
    ("font-stretch" "normal")
    ("font-size")
    ("font-family"))
  "CSS properties we won't include.
If we include a list, we only ignore this property for these values.")

(defun jhfy-html-enkludge-buffer ()
  "Like `hfy-html-enkludge-buffer'.
Does work to escape characters like [\"<>] which we don't want in HTML.
Also marks spaces and newlines to be escaped later.
Marks text with  `jhfy-quoteme' property.

See also `jhfy-html-quote-map' which lists regexes to be escaped."
  (dolist (pair jhfy-html-quote-map)
    (pcase-let* ((`(,reg ,replacement) pair)
		 (regex (rx-to-string reg)))
      (save-excursion
	(goto-char (point-min))
	(while (re-search-forward regex nil t)
	  (unless (get-text-property (match-beginning 0) 'jhfy-quoteme)
	    (let* ((str (match-string 0))
		   (beg (match-beginning 0))
		   (res (thread-first
			  (if (stringp replacement)
			      replacement
			    (funcall replacement str (match-beginning 0)))
			  ;; This is so the same property isn't "eq"
			  (copy-sequence))))
	      (put-text-property beg (point) 'jhfy-quoteme res))))))))

(defun jhfy-html-dekludge-buffer ()
  "Transform all characters marked with the `jhfy-quoteme' property, replacing them with the text in the property.

See also `jhfy-html-enkludge-buffer'."
  (let (match)
    (save-excursion
      (goto-char (point-min))
      (while (setq match (text-property-search-forward 'jhfy-quoteme))
	(goto-char (prop-match-beginning match))
	(delete-region (prop-match-beginning match) (prop-match-end match))
	(insert (prop-match-value match))))))

(defun jhfy--fontify-to-html-string (css-sheet css-map)
  "Takes in current buffer, returns fontified HTML as a string.
This HTML doesn't include the <code> block."
  (let ((temp-buffer (generate-new-buffer " *temp*" t)))
    (copy-to-buffer temp-buffer (point-min) (point-max))

    (with-current-buffer temp-buffer
      (unwind-protect
	  (progn
	    (remove-list-of-text-properties (point-min) (point-max)
					    hfy-ignored-properties)
	    (setq buffer-invisibility-spec nil)

	    (jhfy-html-enkludge-buffer)

	    (dolist (point-face css-map)
	      (let ((pt (car point-face))
		    (fn (cdr point-face)))
		(goto-char pt)
		(if (eq 'end fn)
		    (funcall hfy-end-span-handler)
		  (funcall hfy-begin-span-handler (hfy-lookup fn css-sheet) nil nil nil))))

	    (jhfy-html-dekludge-buffer)
	    (buffer-substring-no-properties (point-min) (point-max)))
	(and (buffer-name temp-buffer)
	     (kill-buffer temp-buffer))))))

;;;###autoload
(defun jmm-htmlfontify-buffer (beg end)
  "Kind of like `htmlfontify-buffer', but generates HTML that doesn't need to be inside a <pre> tag.
Unlike `htmlfontify-buffer', it doesn't handle invisible regions or etags links.
You will probably want to `untabify', because I don't handle tabs correctly.
If the region is active, only htmlfontify the region.
"
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end))
		 (list (point-min) (point-max))))
  (let ((oldbuf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring oldbuf beg end)
      (let* ((html-buffer (generate-new-buffer "*jmm-hfy*"))
	     ;; This overrides something when compiling the stylesheet
	     (hfy-face-to-css #'jhfy-face-to-css-default)
	     (css-sheet (hfy-compile-stylesheet))
	     (css-map (hfy-compile-face-map))
	     (html (jhfy--fontify-to-html-string css-sheet css-map)))

	(with-current-buffer html-buffer
	  (insert "<!DOCTYPE html>\n<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en-US\">\n<head>\n<meta charset=\"UTF-8\"/>\n<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/>\n")
	  (insert "<style>\n")
	  (insert (jhfy-sprintf-stylesheet css-sheet))
	  (insert "\n</style>\n")
	  (insert "</head>\n<body>\n")
	  (insert "<code class=\"fontify\">\n")
	  (insert html)
	  (insert "\n</code>\n")
	  (insert "</body>\n</html>")
	  (goto-char (point-min)))
	(pop-to-buffer html-buffer)))))

;;;###autoload
(defun jmm-htmlfontify-string (string)
  "Fontifies a string, returning a cons of HTML and CSS.
Like `jmm-htmlfontify-buffer'."
  (let ((oldbuf (current-buffer)))
    (with-temp-buffer
      (insert string)
      (let* (;; This overrides something when compiling the stylesheet
	     (hfy-face-to-css #'jhfy-face-to-css-default)
	     (css-sheet (hfy-compile-stylesheet))
	     (css-map (hfy-compile-face-map))
	     (html (jhfy--fontify-to-html-string css-sheet css-map))
	     (css (jhfy-sprintf-stylesheet css-sheet)))
	(cons html css)))))

(defun jhfy--ignore-style-p (property value)
  (let ((ignorevals (alist-get property jhfy-ignore-styles 'jhfy--not-found nil #'equal)))
    (unless (eq ignorevals 'jhfy--not-found)
      (if (null ignorevals)
	  t ;; We're ignoring any value
	(member (if (stringp value) value (format "%S" value)) ignorevals)))))

(defun jhfy-face-to-css-default (fn)
  "See `hfy-face-to-css-default'."
  (let* ((css-list (hfy-face-to-style fn))
         (seen nil)
         (css-text
          (mapconcat
           (lambda (x)
	     (pcase-let ((`(,property . ,value) x))
	       (when property
		 (unless (or (member property seen)
			     (jhfy--ignore-style-p property value))
		   (push property seen)
		   (format " %s: %s;\n" property value)))))
	   css-list)))
    (cons (hfy-css-name fn) (format "{\n%s}" css-text)))) 

(defun jhfy-sprintf-stylesheet (css)
  "Like `hfy-sprintf-stylesheet', but doesn't do any link stuff.
Also assumes a base class of \".fontify\"."
  (with-temp-buffer
    ;; (insert "<style type=\"text/css\">\n")
    (let ((compressed-rules (seq-group-by #'cddr css)))
      (dolist (stylegroup compressed-rules)
	(let ((rules (car stylegroup))
	      (classes (mapcar #'cadr (cdr stylegroup))))
	  (insert
	   (mapconcat
	    (lambda (class)
	      (format ".fontify span.%s" class))
	    classes
	    ",\n"))
	  (insert (format" %s\n" rules)))))
    ;; (insert "</style>\n")
    (buffer-string)))

;;;###autoload
(defun jmm-htmlfontify-css-buffer ()
  "Get the CSS for fontifying the buffer."
  (interactive)
  (let* ((new-buffer (generate-new-buffer "*jmm-hfy-css*"))
	 (hfy-face-to-css #'jhfy-face-to-css-default)
	 (css-sheet (hfy-compile-stylesheet)))
    (switch-to-buffer new-buffer)
    (insert (jhfy-sprintf-stylesheet css-sheet))))

(defun jhfy-dom-texts (node)
  "Like `dom-texts', but ignores certain spaces."
  (cond
   ((eq (dom-tag node) 'br) "\n")
   (t
    (mapconcat
     (lambda (elem)
       (cond
	((stringp elem)
	 (replace-regexp-in-string "\n" " " elem))
	(t
	 (jhfy-dom-texts elem))))
     (dom-children node)))))

(defun jhfy-unfontify-tag ()
  "Unfontify the tag at point."
  (interactive)
  (let* ((new-buffer (generate-new-buffer "*jmm-hfy-edit*"))
	 (dom (xml-parse-tag)))
    (with-current-buffer new-buffer
      (insert (jhfy-dom-texts dom))
      (goto-char (point-min))
      (save-excursion (while (re-search-forward (rx bol (1+ " ")) nil t) (replace-match "")))
      (save-excursion (while (re-search-forward (rx " " (1+ " ")) nil t) (replace-match " ")))
      (save-excursion (while (re-search-forward " " nil t) (replace-match " "))))
    (pop-to-buffer
     new-buffer
     '((display-buffer-below-selected display-buffer-at-bottom)
       (inhibit-same-window . t)
       (window-height . fit-window-to-buffer)))))

(provide 'jmm-htmlfontify)
;;; jmm-htmlfontify.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("jhfy-" . "jmm-hfy-"))
;; End:
