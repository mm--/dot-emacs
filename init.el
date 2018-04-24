;; Speed up load time by preventing GC pauses
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(unless (>= emacs-major-version 27)
  (package-initialize))

;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/org") ;Newer version of org mode
;; (require 'org)
;; (org-babel-load-file "~/.emacs.d/jmm-emacs.org")
;; (org-babel-load-file "~/.emacs.d/jmm-org-config.org")
;; (org-babel-tangle-file "~/.emacs.d/jmm-emacs.org" "~/.emacs.d/jmm-emacs.el")
;; (org-babel-tangle-file "~/.emacs.d/jmm-org-config.org" "~/.emacs.d/jmm-org-config.el")
;; (byte-compile-file "~/.emacs.d/jmm-emacs.el")
;; (load-file "~/.emacs.d/jmm-emacs.elc")



(if (or (file-newer-than-file-p "~/.emacs.d/jmm-emacs.org" "~/.emacs.d/jmm-emacs.el")
	(file-newer-than-file-p "~/.emacs.d/jmm-org-config.org" "~/.emacs.d/jmm-org-config.el"))
    (progn
      (require 'org)
      (org-babel-load-file "~/.emacs.d/jmm-emacs.org")
      (org-babel-load-file "~/.emacs.d/jmm-org-config.org"))
  (load-file "~/.emacs.d/jmm-emacs.el")
  (eval-after-load 'org
    '(progn (load-file "~/.emacs.d/jmm-org-config.el"))))

(jmm/load-personal-files)
