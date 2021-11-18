;;;###autoload
(defun jmm/notmuch-search-org ()
  "Search my org mode project for the notmuch ID, to see if I have something already entered somewhere."
  (interactive)
  (let* ((messageid (notmuch-show-get-message-id))
	 (default-directory "~/org")
	 (xref-auto-jump-to-first-xref nil)
	 (newbuf (project-find-regexp messageid)))
    (switch-to-buffer newbuf)
    ;; I think there's some issue with the buffer not "having" all matches yet at this point.
    (current-buffer)
    (goto-char (point-min))
    (xref-next-line)
    ;; Don't know why this changes things
    (xref-next-line)
    ;; (current-buffer)
    ;; (xref-goto-xref nil)
    ;; (org-reveal)
    (message "Hello there")))

;;;###autoload
(defun jmm/notmuch-search-org-jump ()
  "Another way of trying to find an org entry for an email.
Like `jmm/notmuch-search-org', but jumps directly to the first one."
  (interactive)
  (let* ((messageid (notmuch-show-get-message-id))
	 (default-directory "~/org")
	 (xref-auto-jump-to-first-xref nil)
	 (orgfiles (->> (project-files (project-current nil "~/org"))
			(--filter (string-equal "org" (file-name-extension it)))))
	 (xrefmatches (xref-matches-in-files messageid orgfiles)))
    (let* ((xref (car xrefmatches))
	   (xref--current-item xref))
      (if xref
	  (progn
	    (xref--show-location (xref-item-location xref) t)
	    (org-reveal))
	(user-error "No link found for this notmuch message ID.")))))

(fset 'jmm/notmuch-mark-all-previous-read
      (kmacro-lambda-form [?\C-e ?\C-  ?\C-a ?\M-< ?k ?U ?\C-x ?\C-x] 0 "%d"))

(provide 'notmuch-stuff)
