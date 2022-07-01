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

(provide 'skeleton-test)
