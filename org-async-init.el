;; When asynchronously exporting org-mode files, just load this file.
(package-initialize)
(require 'org)
(require 'ol-notmuch) ;; It looks like this changed from "org-notmuch" to "ol-notmuch" recently.
(setq org-export-with-broken-links t)
(load-file "~/.emacs.d/jmm-emacs.el")
(load-file "~/.emacs.d/jmm-org-config.el")
(jmm/load-personal-files)
