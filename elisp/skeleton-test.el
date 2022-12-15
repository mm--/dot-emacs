;; -*- lexical-binding: t; -*-
;; Unorganized skeleton and abbreviation utilities.
(require 'nxml-mode)
(require 'thunk)

(defvar jmm-skeleton-last-defined nil)

;;;###autoload
(define-skeleton skeleton-skeleton
  "Define a skeleton, using a skeleton"
  "Skeleton name: "
  "(define-skeleton " str \n
  (prin1-to-string (read-string "Documentation: ")) \n
  '(setq v1 (read-string "Prompt: "))
  (if (string-empty-p v1) "nil"
    (prin1-to-string v1))
  \n
  _
  ")")

;;;###autoload
(defun jmm-edit-abbrevs ()
  "Just like `edit-abbrevs', but doesn't use `switch-to-buffer'."
  (interactive)
  (let* ((table-name (abbrev-table-name local-abbrev-table))
	 (buf (prepare-abbrev-list-buffer)))
    (pop-to-buffer buf)
    (when (and table-name
               (search-forward
                (concat "(" (symbol-name table-name) ")\n\n") nil t))
      (goto-char (match-end 0)))))

(defun jmm-edit-abbrevs-for (table-name)
  "Edit abbreviations for a given TABLE-NAME.
Basically just `edit-abbrevs' for a provided table. "
  (let* ((buf (prepare-abbrev-list-buffer)))
    (pop-to-buffer-same-window buf)
    (when (and table-name
               (search-forward
                (concat "(" (symbol-name table-name) ")\n\n") nil t))
      (goto-char (match-end 0)))))

(defun jmm-skeleton--default-name (table-name abbr)
  "Suggest a default skeleton name for a local abbrevation table and abbreviation."
  (format
   "skeleton/%s/%s"
   (thread-last
     (symbol-name table-name)
     (string-remove-suffix "-abbrev-table")
     (string-remove-suffix "-mode"))
   abbr))

;; TODO: Prompt for abbreviation tables.
;;;###autoload
(defun jmm-add-local-skeleton ()
  "Open this file, try to find the right place to add a skeleton.

Prompts for an abbreviation name, skeleton name.
Leads you through the process of defining a skeleton.
Once you're done editing the skeleton, evaluate it and `exit-recursive-edit'.
Then the abbreviation will be inserted in the `edit-abbrevs' buffer.
"
  (interactive)
  (let ((table-name (abbrev-table-name local-abbrev-table)))
    (find-library-other-window "skeleton-test")
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
	    (dlet ((m1 markstring))
	      (skeleton-proxy-new
	       '(nil
		 ?\f ?\n
		 ";;;;;;;;;;\n"
		 ";; " m1 ?\n
		 "\n\n"
		 _
		 "\n\n\n")))))
	(let* ((skelabbrev (read-string "Abbreviation: "))
	       (skeldefault (jmm-skeleton--default-name table-name skelabbrev))
	       (skelname (read-string (format-prompt "Skeleton name" skeldefault)
				      nil nil skeldefault)))
	  (insert ";;;###autoload\n")
	  (setq jmm-skeleton-last-defined skelname)
	  (kill-new skelname)
	  (skeleton-skeleton skelname)
	  (recursive-edit)
	  (jmm-edit-abbrevs-for table-name)
	  (insert "\n")
	  (skeleton-edit-abbrev-last-skeleton skelabbrev)
	  (save-excursion (insert "\n")))
	))))

(defvar-local jmm-minibuffer-lazy-history nil)

(defun jmm-minibuffer-add-lazy-history ()
  "Used as a `minibuffer-default-add-function'.
Pops elements off of `jmm-minibuffer-lazy-history', evaluating them with `thunk-force'"
  (if-let* ((val (pop jmm-minibuffer-lazy-history)))
      (progn
	(when jmm-minibuffer-lazy-history ;; If there are more items left
	  (setq minibuffer-default-add-done nil))
	(append minibuffer-default (list (thunk-force val))))
    minibuffer-default))

(defun jmm-skeleton-prompt (prompt &optional futurehistory)
  "Prompt for string, returning nil if nothing entered.
FUTUREHISTORY is a list of strings or thunks/functions.
Functions will get lazily evaluated as if by thunk-force/funcall."
  (let* ((immediatefuturehistory (seq-take-while #'stringp futurehistory))
	 (lazyfuturehistory (seq-drop futurehistory (length immediatefuturehistory)))
	 (res (minibuffer-with-setup-hook
		 (lambda ()
		   (setq-local jmm-minibuffer-lazy-history lazyfuturehistory)
		   (setq-local minibuffer-default-add-function #'jmm-minibuffer-add-lazy-history))
		(read-from-minibuffer prompt
				      nil nil nil nil
				      immediatefuturehistory))))
    (unless (string-empty-p res)
      res)))

(defmacro jmm-skeleton-unless-wrapping (&rest body)
  "If we can wrap, return \"_\" skeleton element.
Otherwise run body.
If body returns nil, return \"_\" anyway so we leave the skeleton-point there."
  `(if skeleton-regions
       '_
     (or (progn ,@body) '_)))

;; TODO: Move elsewhere
(defun jmm--get-url-html-title (url)
  "Get the title of a webpage from URL"
  (with-temp-buffer
    (url-insert-file-contents url)
    (let* ((dom (libxml-parse-html-region (point-min) (point-max))))
      (when-let* ((title-elements (dom-by-tag dom 'title)))
	(dom-text (car title-elements))))))


;;;;;;;;;;
;; General skeletons

;;;###autoload
(define-skeleton skeleton-date
  "Just insert the current date"
  nil
  (format-time-string "%Y-%m-%d"))

;;;###autoload
(define-skeleton skeleton-time
  "Just insert the current time"
  nil
  (format-time-string "%H:%M"))

;;;###autoload
(define-skeleton skeleton-datetime
  "Just insert the current date time"
  nil
  (format-time-string "%Y-%m-%d %H:%M"))

;;;###autoload
(define-skeleton skeleton/global/org-read-date
  "Insert the date using `org-read-date'"
  nil
  (format-time-string "%Y-%02m-%02d" (org-read-date nil t nil "Date: ")))

;;;###autoload
(define-skeleton skeleton/global/org-read-date-time
  "Insert a date and time using `org-read-date'"
  nil
  (format-time-string "%Y-%02m-%02d %H:%M" (org-read-date t t nil "Date: ")))

;;;###autoload
(define-skeleton skeleton/global/relk
  "A relative file path from kill ring"
  nil
  (file-relative-name (expand-file-name (car kill-ring))))



;;;;;;;;;;
;; XML skeletons

;;;###autoload
(define-skeleton skeleton/svg-skeleton
  "Skeleton for an SVG"
  nil
  "<svg
   width=\"\"
   height=\"\"
   viewBox=\"\"
   version=\"1.1\"
   xmlns:xlink=\"http://www.w3.org/1999/xlink\"
   xmlns=\"http://www.w3.org/2000/svg\"
   xmlns:svg=\"http://www.w3.org/2000/svg\">
" _ "
</svg>
")



;;;;;;;;;;
;; Nix skeletons

;; TODO: Move `jmm/nix-get-keys-of' elsewhere
;;;###autoload
(defun jmm/nix-get-keys-of (attrset)
  "Get the keys of ATTRSET in nixpkgs.
Example: you get get a list of all rPackages."
  (json-parse-string
   (shell-command-to-string
    (format "nix eval --raw \"(with import <nixpkgs> {}; builtins.toJSON (builtins.attrNames %s))\""
	    (shell-quote-argument attrset)))
   :array-type 'list))

;;;###autoload (autoload 'nix-myenv "skeleton-test" nil t nil)
(define-skeleton nix-myenv
  "Kind of like what you'd do with a shell.nix"
  nil
  "{ pkgs ? import <nixpkgs> {} }:" \n
  "" \n
  "with pkgs; {" \n
  "myenv = buildEnv {" \n
  "name = \"RustEnv\";" \n
  "paths = [" \n
  "rustc" \n
  "cargo" \n
  "gcc" \n
  "rustfmt" \n
  "rustPackages.clippy" \n
  "];" > \n
  "};" > \n
  "}" >)

(defvar jmm/r-suggested-packages
  (lazy-completion-table jmm/r-suggested-packages
			 (lambda ()
			   (jmm/nix-get-keys-of "rPackages")))
  "Completion table for R packages, for a skeleton.")

;;;###autoload (autoload 'nix-default-r-skeleton "skeleton-test" nil t nil)
(define-skeleton nix-default-r-skeleton
  "A skeleton for default.nix that implements a wrapped R env"
  "This prompt is ignored."
"{ pkgs ? import <nixpkgs> {}
, ... }:

with pkgs;
{
  myenv = let
    my-R-packages = with rPackages; [
      R
      tidyverse
      jsonlite
      data_table
      rstan
      ggplot2"
;; Not sure why there needs to be whitespace below
;; Otherwise it seems to want to interpret it as Elisp
  ((completing-read "R package: " jmm/r-suggested-packages nil nil) \n str >)
"
    ];
    newR = rWrapper.override {
      packages = my-R-packages;
    };
  in
    buildEnv {
      name = \"myenv\";
      paths = [
        newR
        gcc                     # For rstan
        gnumake                 # Ditto
      ];
    };
}"
)

(defvar jmm/latex-suggested-packages
  (lazy-completion-table jmm/latex-suggested-packages
			 (lambda ()
			   (jmm/nix-get-keys-of "texlive")))
  "Completions for LaTeX packages, for a skeleton.")

(defvar jmm/nix-all-suggested-packages
  (lazy-completion-table jmm/nix-all-suggested-packages
			 (lambda ()
			   (jmm/nix-get-keys-of "pkgs")))
  "Completion table of all nixpkgs.")

;;;###autoload (autoload 'nix-default-latex-skeleton "skeleton-test" nil t nil)
(define-skeleton nix-default-latex-skeleton
  "A skeleton for default.nix that brings in LaTeX."
  "This prompt is ignored."
  "{ pkgs ? import <nixpkgs> {}
, ... }:

with pkgs; rec {
texenv = texlive.combine {
   inherit (texlive) scheme-medium standalone todonotes
      glossaries
      glossaries-extra"
;; Not sure why there needs to be whitespace below
;; Otherwise it seems to want to interpret it as Elisp
  ((completing-read "LaTeX package: " jmm/latex-suggested-packages nil nil) \n str >)
";
};

   myenv = buildEnv {
      name = \"texpackages\";
      paths = [
        texenv
        biber"
;; Still need whitespace here.
  ((completing-read "Package: " jmm/nix-all-suggested-packages nil nil) \n str >)
"
      ];
    };
}
"
'(indent-region (point-min) (point-max))
)


;;;;;;;;;;
;; HTML skeletons
;; including:
;; (jmm-xhtml-mode-abbrev-table)

(defun jmm/html-comment-line ()
  "Comment the currrent line."
  (comment-region (save-excursion (back-to-indentation) (point)) (point))
  (end-of-line)
  nil)

(defun jmm/html-prompt1 (prompt)
  "Prompt for text to put between last tags."
  (save-excursion
    (backward-sexp)
    (nxml-down-element)
    (insert (xml-escape-string (skeleton-read prompt nil t))))
  nil)

(defun jmm/html-prompt (tags)
  "Return a skeleton to place some text between tags."
  `(nil
    ,tags
    '(jmm/html-prompt1 ,(format "%s: " tags))
    >
    \n))

;;;###autoload (autoload 'html-skeleton "skeleton-test")
(define-skeleton html-skeleton
  "Josh's simple skeleton for HTML stuff."
  ""
"<!DOCTYPE html>
<html lang=\"en-US\">
    <head>
	<meta charset=\"UTF-8\"/>
	<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/>" >! \n
	"<meta http-equiv=\"Content-Security-Policy\" content=\"default-src https:\"/>" >! \n
	"<link rel=\"icon\" href=\"/favicon.png\"/>" >! \n
"<meta name=\"referrer\" content=\"origin\"/>" \n
(nil "<meta name=\"author\" content=\"" (skeleton-read "Author: " nil t) "\"/>" > \n)
(jmm/html-prompt "<title></title>")
"<style>" "</style>" \n
"<!-- Should make this async? -->
	<link rel=\"stylesheet\" href=\"css/style.css\" />
    </head>
    <body>" \n
(jmm/html-prompt "<header></header>")
"<main>" >  _ "
    </main>
    </body>
</html>
")

;; A very hacky way of adding my own custom skeleton elements.
(defadvice html-skeleton (before add-further activate)
  (setq-local skeleton-further-elements
             '((>! '(jmm/html-comment-line))
               )))

(define-skeleton html-ul-skeleton
  "A simple skeleton for ul elements."
  nil
  "<ul>" > \n
  ("Element: " "<li>" str "</li>" > \n)
  "</ul>" >)

(define-skeleton html-li-skeleton
  "A simple skeleton for more li elements."
  nil
  ("Element: " "<li>" str "</li>" > \n)
  '(delete-blank-lines))

(define-skeleton skeleton-html-figure
  "Inserts a basic figure"
  nil
  '(setq v1 (point))
  "<figure id=\"" (read-string "ID: ") "\">
  <img src=\"" (read-string "Source: ") "\"/>
  <figcaption>" '(setq v2 (point-marker)) "</figcaption>
</figure>"
  '(indent-region v1 (point))
  '(goto-char v2))

;;;###autoload
(define-skeleton skeleton/jmm-xhtml/notehtml
  "Skeleton for an HTML page for notes"
  "Title: "
  "<!DOCTYPE html>
<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en-US\">
 <head>
  <meta charset=\"UTF-8\"/>
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\"/>
  <!-- <meta http-equiv=\"Content-Security-Policy\" content=\"default-src https:\"/> -->
  <meta name=\"referrer\" content=\"no-referrer\"/>
  <meta name=\"author\" content=\"Josh Moller-Mara\"/>
  <title>" str "</title>
  <link rel=\"stylesheet\" href=\"./style.css\" type=\"text/css\" title=\"Default style\"/>" \n
  (when (y-or-n-p "Skewer?") '(nil "<script src=\"http://localhost:8081/skewer\"></script>" \n))
  "</head>
 <body>
  <header>
   <h1>" str "</h1>
  </header>
  <main>
  </main>
 </body>
</html>
<!-- Local Variables: -->
<!-- mode: jmm-xhtml -->
<!-- nxml-child-indent: 1 -->
<!-- word-wrap: t -->
<!-- End: -->
")

;;;###autoload
(define-skeleton skeleton/jmm-xhtml/calevs
  "List of calendar events for my personal calendar."
  nil
  "<ul class=\"events\">" > \n
  "<li>" _ "</li>" \n
  "</ul>" >
  )


;;;###autoload
(define-skeleton skeleton/jmm-xhtml/frag
  "Add a slide fragment to a node."
  "Frag ID? "
  '(jmm-xhtml-add-class "fragment")
  "data-fragment-index=\"" str "\"")


;;;###autoload
(define-skeleton skeleton/jmm-xhtml/afile
  "Insert a link to a local file from kill ring"
  nil
  '(setq v1 (current-kill 0))
  "<a href=\"" (xml-escape-string (format "file://%s" v1)) "\">"
  ;; I only want to prompt if I'm not wrapping.
  ;; If we prompt, we shouldn't set the interesting point.
  ;; _ | (jmm-skeleton-prompt "Link text" (list (file-name-nondirectory v1) v1))
  (jmm-skeleton-unless-wrapping
   (jmm-skeleton-prompt "Link text: " (list (file-name-nondirectory v1) v1)))
  "</a>")


;;;###autoload
(define-skeleton skeleton/jmm-xhtml/arelk
  "Insert a link to relative file from kill ring"
  "Link text: "
  "<a href=\"" (xml-escape-string (file-relative-name (expand-file-name (car kill-ring)))) "\">" str "</a>")

;;;###autoload
(define-skeleton skeleton/jmm-xhtml/akill
  "Insert link from current kill."
  _
  '(setq v1 (current-kill 0))
  "<a href=\"" (xml-escape-string v1) "\">"
  '(setq v2 (ignore-errors (url-generic-parse-url v1)))
  (jmm-skeleton-unless-wrapping
   (jmm-skeleton-prompt "Link text: "
			(seq-uniq
			 (seq-remove
			  #'null
			  (list
			   (xml-escape-string v1)
			   (ignore-errors (url-host v2))
			   (ignore-errors (url-domain v2))
			   (lambda () (jmm--get-url-html-title v1)))))))
  "</a>")



;;;###autoload
(define-skeleton skeleton/jmm-xhtml/stodo
  "Add a todo span with current date"
  nil
  "<span class=\"todo\" data-added=\""
  (xml-escape-string (format-time-string "%Y-%m-%d %H:%M"))
  "\">" (read-string "Todo text: ")  | _ "</span>")


;;;###autoload
(define-skeleton skeleton/jmm-xhtml/aemail
  "Link to local email"
  nil
  '(setq v1 (pop org-stored-links))
  "<a data-orglink=\"" (xml-escape-string (format "[[%s]]" (car v1))) "\" "
  (when-let* ((title (jmm-skeleton-prompt "Title: " (list (cadr v1)))))
    (format "title=\"%s\" " (xml-escape-string title)))
  "href=\""
  (xml-escape-string (save-window-excursion
		       (org-link-open-from-string (car v1))
		       (jmm/notmuch-show-get-gmail-link)))
  "\" rel=\"noreferrer\">"
  (jmm-skeleton-prompt "Link text: " (list (cadr v1)))
  "</a>")

;;;###autoload
(define-skeleton skeleton/jmm-xhtml/br
  "Insert a line break"
  nil
  "<br/>" \n
  > )

;;;###autoload
(define-skeleton skeleton/jmm-xhtml/li1
  "Insert an <li> element </li>"
  nil
  "<li>" _  "</li>")


;;;###autoload
(define-skeleton skeleton/jmm-xhtml/slidesvg
  "Embed a slide SVG file from current kill."
  nil
  > "<div class=\"slidecontainer\">" \n
  "<embed src=\"" (xml-escape-string (file-relative-name (expand-file-name (car kill-ring)))) "\" />" \n
  "</div>" > )


;;;###autoload
(define-skeleton skeleton/jmm-xhtml/sliden
  "Slide note. A simple div."
  nil
  "<div class=\"slidenotes\">" \n
  _
  \n
  "</div>" > )


;;;###autoload
(define-skeleton skeleton/jmm-xhtml/jmmfr
  "Add a presentation slide fragment."
  nil
  "<jmmfr:setclass"
  (when-let* ((begin (jmm-skeleton-prompt "Begin: ")))
    (format " begin=\"%s\"" begin))
  (when-let* ((end (jmm-skeleton-prompt "End: ")))
    (format " end=\"%s\"" end))
  " classes=\""
  ;; TODO: Autocomplete classes
  (xml-escape-string (jmm-skeleton-prompt "Classes: " (list "visible")))
  "\" />")


;;;###autoload
(define-skeleton skeleton/jmm-xhtml/dets
  "Insert a <details> element"
  nil
  "<details>" \n
  "<summary>" (jmm-skeleton-prompt "Summary: ") "</summary>" \n
  _ \n
  "</details>" >)



;;;;;;;;;;
;; CSS skeletons
;; (css-mode-abbrev-table)

;;;###autoload
(define-skeleton skeleton/css/notecss
  "A basic skeleton for CSS styles for notes I take."
  ;; Probably will add more stuff later.
  nil
  "body {
    font: 1rem 'Fira Sans', sans-serif;
}
h1 {
    text-align: center;
}
")


;;;;;;;;;;
;; Javascript skeletons
;; (js2-mode-abbrev-table)

;;;###autoload
(define-skeleton skeleton/js2/qs
  "Skeleton for \"querySelector\""
  nil
  "querySelector(\"" (jmm-skeleton-unless-wrapping
		      (jmm-skeleton-prompt "querySelector text: ")) "\")" _ )

;;;###autoload
(define-skeleton skeleton/js2/qsa
  "Skeleton for \"querySelectorAll\""
  nil
  "querySelectorAll(\"" (jmm-skeleton-unless-wrapping
			 (jmm-skeleton-prompt "querySelector text: ")) "\")" _ )

;;;###autoload
(define-skeleton skeleton/js2/dqs
  "Skeleton for \"document.querySelector\""
  nil
  "document.querySelector(\"" (jmm-skeleton-unless-wrapping
			       (jmm-skeleton-prompt "QuerySelector text: ")) "\")" _)

;;;###autoload
(define-skeleton skeleton/js2/0cl
  "Insert a JavaScript console.log"
  nil
  "console.log(" (jmm-skeleton-unless-wrapping
		    (jmm-skeleton-prompt "console.log text: ")) ")" _ )



;;;;;;;;;;
;; Eshell skeletons
;; (eshell-mode-abbrev-table)

;;;###autoload
(define-skeleton skeleton-eshell-dmfor
  "Set up looping over the list of marked files"
  nil
  "for x in $dm { " _ "$x }")

;;;###autoload
(define-skeleton skeleton/eshell/ffmeta
  "Add metadata to media file using ffmpeg"
  nil
  ;; Bug: Quitting inside subskeletons causes `eshell-last-output-end' to get
  ;; reset to beginning of line
  '(narrow-to-region (point) (point-max))
  "ffmpeg -i " (shell-quote-argument (setq v1 (file-relative-name (expand-file-name (read-file-name "File: ")))))
  " -metadata title=\"" (read-string "Title: ")"\""
  ("Field: " " -metadata " str "=\"" (read-string "Value: ")"\"")
  " -c copy "
  (shell-quote-argument (file-name-sans-extension v1))
  _ "." (file-name-extension v1)
  '(widen))


(defun jmm-get-pulseaudio-ids (source-or-sink)
  "Return an alist from PulseAudio names to IDs.
SOURCE-OR-SINK should be either 'source or 'sink."
  (let ((arg (pcase source-or-sink
	       ('sink "sinks")
	       ('source "sources")
	       (t (error "Expecting either 'source or 'sink")))))
    (thread-last
      (split-string (shell-command-to-string (format "pactl list short %s" arg)) "\n" t)
      (mapcar (lambda (x) (split-string x split-string-default-separators t)))
      (mapcar (pcase-lambda (`(,num ,name ,_))
		(cons name num))))))

(add-to-list 'completion-category-defaults
	     '(jmm-pulseaudio-name (styles orderless substring)))

(defun jmm-read-pulseaudio (source-or-sink prompt)
  "Read a PulseAudio source or sink name.
SOURCE-OR-SINK should be either 'source or 'sink."
  (let* ((alist (jmm-get-pulseaudio-ids source-or-sink))
	 (collection (lambda (string pred action)
		       (if (eq action 'metadata)
			   `(metadata (category . jmm-pulseaudio-name))
			 (complete-with-action action alist string pred)))))
    (shell-quote-argument (completing-read (format "%s: " prompt) collection))))

;;;###autoload
(define-skeleton skeleton/eshell/ffrecord
  "Make a recording of computer speakers and microphone."
  "Filename: "
  "ffmpeg -fflags nobuffer -flags low_delay "
  "-f pulse -i " (jmm-read-pulseaudio 'source "Left side") " "
  "-f pulse -i " (jmm-read-pulseaudio 'source "Right side") " "
  "-filter_complex \"[0:a][1:a]amerge=inputs=2,pan=stereo|c0<c0+c1|c1<c2+c3[a]\" -map \"[a]\" "
  "-fflags +genpts " str)

;;;###autoload
(define-skeleton skeleton/eshell/ytmv
  "Download a music video using yt-dlp"
  nil
  '(setq str (let ((default (car kill-ring)))
	       (read-string (format-prompt "URL" default) nil nil default)))
  "yt-dlp --no-mtime --no-embed-metadata --format 243+251 "
  (format "\"%s\"" str))


;;;###autoload
(define-skeleton skeleton/eshell/ffprobe
  "Get the metadata of a file using ffprobe"
  nil
  "ffprobe -hide_banner -i "
  (shell-quote-argument (file-relative-name (expand-file-name (read-file-name "File: ")))))



;;;;;;;;;;
;; Emacs lisp skeletons
;; (emacs-lisp-mode-abbrev-table)

;;;###autoload
(define-skeleton skeleton/emacs-lisp/keymap-global-set
  "Make it easier to bind a key"
  "Keybinding: "
  "(keymap-global-set \"" str "\" #'" (symbol-name (read-command "Command: ")) ")")

(defun jmm-make-keymap-table ()
  "Returns a completion table for loaded keymaps."
  (thunk-let ((maps (let (maps)
		      (mapatoms
		       (lambda (sym)
			 (when (and (boundp sym)
				    (keymapp (symbol-value sym)))
			   (push sym maps)))
		       obarray)
		      maps)))
    (lambda (string pred action)
      (if (eq action 'metadata)
	  '(metadata
	    (category . keymap))
	(complete-with-action action maps string pred)))))

(defun jmm-make-command-table ()
  "Returns a completion table for loaded commands."
  (thunk-let ((commands (let (l)
			  (mapatoms
			   (lambda (sym)
			     (when (commandp sym)
			       (push sym l)))
			   obarray)
			  l)))
    (lambda (string pred action)
      (if (eq action 'metadata)
	  '(metadata
	    (category . command))
	(complete-with-action action commands string pred)))))

;;;###autoload
(define-skeleton skeleton/emacs-lisp/defkey
  "Expand a `define-key' call.
Prompts for keymap name."
  nil
  "(define-key "
  (completing-read "Keymap: " (jmm-make-keymap-table)) " "
  "(kbd \"" (read-string "Key: ") "\") "
  "#'" (completing-read "Command: " (jmm-make-command-table)) ")")

;; MAYBE: Should I add command modes to other skeletons?
;; Note: This won't add the command modes to the autoload definition, though.
(put 'skeleton/emacs-lisp/defkey 'command-modes '(emacs-lisp-mode))


;;;;;;;;;;
;; (edit-abbrevs-mode-abbrev-table)

;;;###autoload
(define-skeleton skeleton-edit-abbrev-last-skeleton
  "Make an abbreviation for the last defined skeleton"
  "Abbreviation to expand: "
  "\"" str "\" 0 \"\" " jmm-skeleton-last-defined)



;;;;;;;;;;
;; (timebox-xml-mode-abbrev-table)


;;;###autoload
(define-skeleton skeleton/timebox-xml/stm
  "Span with current time."
  nil
  "<span time=\""
  (xml-escape-string (format-time-string "%H:%M:%S"))
  "\">" _ "</span>")



(provide 'skeleton-test)
