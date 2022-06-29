(setq auto-insert-directory (locate-user-emacs-file "auto-insert/"))

;; I put this before the auto-insert-alist because 'auto-insert-alist
;; isn't immediately defined.
(auto-insert-mode t)

;; TODO: What's the proper way to add to variables not yet defined?
;; Do I make an autoload declaration for it? Or do I use customize?
(add-to-list 'auto-insert-alist
	     '(("\\.envrc\\'" . "direnv envrc")
	       . "envrc"))

(provide 'jmm-autotype)
