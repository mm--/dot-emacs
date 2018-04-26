;; Bootstrap initialization stuff

;; Speed up load time by preventing GC pauses
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))

;; Package-initialize is automatically called in Emacs 27
(unless (>= emacs-major-version 27)
  (package-initialize))

;; These comments are here for me to manually run with C-x C-e
;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/org") ;Newer version of org mode
;; (require 'org)
;; (org-babel-load-file "~/.emacs.d/jmm-emacs.org")
;; (org-babel-load-file "~/.emacs.d/jmm-org-config.org")
;; (org-babel-tangle-file "~/.emacs.d/jmm-emacs.org" "~/.emacs.d/jmm-emacs.el")
;; (org-babel-tangle-file "~/.emacs.d/jmm-org-config.org" "~/.emacs.d/jmm-org-config.el")
;; (byte-compile-file "~/.emacs.d/jmm-emacs.el")
;; (load-file "~/.emacs.d/jmm-emacs.elc")

;; Don't load my org-config automatically, as it slows startup time
;; It'll be automatically loaded when opening org files.
;; However, we *do* need it if there are changes in any of the
;; configs, since we'll need to re-tangle them.

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
