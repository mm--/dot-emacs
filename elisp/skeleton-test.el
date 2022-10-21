;; -*- lexical-binding: t; -*-
;; Unorganized skeleton and abbreviation utilities.
(require 'nxml-mode)

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


;;;;;;;;;;
;; Javascript skeletons
;; (js2-mode-abbrev-table)

;;;###autoload
(define-skeleton skeleton/js2/qs
  "Skeleton for \"querySelector\""
  nil
  "querySelector(\"" _ "\")")

;;;###autoload
(define-skeleton skeleton/js2/qsa
  "Skeleton for \"querySelectorAll\""
  nil
  "querySelectorAll(\"" _ "\")")

;;;###autoload
(define-skeleton skeleton/js2/dqs
  "Skeleton for \"document.querySelector\""
  nil
  "document.querySelector(\"" _ "\")")


;;;;;;;;;;
;; Eshell skeletons
;; (eshell-mode-abbrev-table)

;;;###autoload
(define-skeleton skeleton-eshell-dmfor
  "Set up looping over the list of marked files"
  nil
  "for x in $dm { " _ "$x }")


;;;;;;;;;;
;; Emacs lisp skeletons
;; (emacs-lisp-mode-abbrev-table)

;;;###autoload
(define-skeleton skeleton/emacs-lisp/keymap-global-set
  "Make it easier to bind a key"
  "Keybinding: "
  "(keymap-global-set \"" str "\" #'" (symbol-name (read-command "Command: ")) ")")


;;;;;;;;;;
;; (edit-abbrevs-mode-abbrev-table)

;;;###autoload
(define-skeleton skeleton-edit-abbrev-last-skeleton
  "Make an abbreviation for the last defined skeleton"
  "Abbreviation to expand: "
  "\"" str "\" 0 \"\" " jmm-skeleton-last-defined)



(provide 'skeleton-test)
