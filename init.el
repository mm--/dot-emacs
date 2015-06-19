
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/org") ;Newer version of org mode
(require 'org)
(org-babel-load-file "~/.emacs.d/jmm-emacs.org")
