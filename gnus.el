(setq gnus-select-method '(nnnil))

(setq gnus-secondary-select-methods '(;; (nnmbox "")
				      ;; TOO SLOW!
				      ;; Probably should set up Dovecot
				      ;; (nnmaildir "nyu"
				      ;; 		 (directory "~/Mail/nyu/")
				      ;; 		 (directory-files nnheader-directory-files-safe)
				      ;; 		 (get-new-mail nil))
				      ))

(require 'mm-url)
(defadvice mm-url-insert (after DE-convert-atom-to-rss () )
  "Converts atom to RSS by calling xsltproc."
  (when (re-search-forward "xmlns=\"http://www.w3.org/.*/Atom\""
               nil t)
    (message "Converting Atom to RSS... ")
    (goto-char (point-min))
    (call-process-region (point-min) (point-max)
             "xsltproc"
             t t nil
             (expand-file-name "~/atom2rss.xsl") "-")
    (goto-char (point-min))
    (message "Converting Atom to RSS... done")))

(ad-activate 'mm-url-insert)

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-number))
