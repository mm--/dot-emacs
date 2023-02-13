(setq auto-insert-directory (locate-user-emacs-file "auto-insert/"))

;; A handy abbreviation if we want to use `insert-file'
(define-abbrev minibuffer-mode-abbrev-table "ains" auto-insert-directory nil
  :system t)

;; I put this before the auto-insert-alist because 'auto-insert-alist
;; isn't immediately defined.
;; FIXME: This will weirdly prompt the first time custom.el is created.
(auto-insert-mode t)
;; TODO: Maybe just disable this and run `auto-insert' manually.

;; TODO: What's the proper way to add to variables not yet defined?
;; Do I make an autoload declaration for it? Or do I use customize?
(add-to-list 'auto-insert-alist
	     '(("\\.envrc\\'" . "direnv envrc")
	       . "envrc"))
(setf (alist-get 'html-mode auto-insert-alist) nil)
(add-to-list 'auto-insert-alist
	     '(("style.css\\'" . "Notes CSS")
	       . "notes-style.css"))

(add-to-list 'auto-insert-alist
	     '(("CACHEDIR.TAG\\'" . "Cache Directory Tag")
	       . "cachedir-tag.txt"))

(provide 'jmm-autotype)
