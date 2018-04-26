;; When asynchronously exporting org-mode files, just load this file.
(package-initialize)
(require 'org)
(require 'org-notmuch)
(setq org-export-with-broken-links t)
(load-file "~/.emacs.d/jmm-emacs.el")
(load-file "~/.emacs.d/jmm-org-config.el")
(jmm/load-personal-files)
