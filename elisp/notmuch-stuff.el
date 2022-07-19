;;;###autoload
(defun jmm/notmuch-search-org ()
  "Search my org mode project for the notmuch ID, to see if I have something already entered somewhere."
  (interactive)
  (let* ((messageid (notmuch-show-get-message-id))
	 (default-directory "~/org")
	 (xref-auto-jump-to-first-xref nil)
	 (newbuf (project-find-regexp (regexp-quote messageid))))
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
	 (xrefmatches (xref-matches-in-files (regexp-quote messageid) orgfiles))
	 (nmatches (length xrefmatches)))
    (let* ((xref (car xrefmatches))
	   (xref--current-item xref))
      (if xref
	  (progn
	    (notmuch-show-tag (list "+inorg"))
	    (xref--show-location (xref-item-location xref) t)
	    (org-reveal)
	    (when (> nmatches 1)
	      (message "Found %d matches for this email." nmatches)))
	(progn
	  (notmuch-show-tag (list "-inorg"))
	  (user-error "No link found for this notmuch message ID."))))))

(fset 'jmm/notmuch-mark-all-previous-read
      (kmacro-lambda-form [?\C-e ?\C-  ?\C-a ?\M-< ?k ?U ?\C-x ?\C-x] 0 "%d"))


(defun jmm/notmuch-get-from ()
  "Get the from email address in notmuch-show-mode.
Get original sender if using Firefox Relay."
  (when (eq major-mode 'notmuch-show-mode)
    (save-match-data
      (let ((mystr (notmuch-show-get-from)))
	(when (or (string-match "\"\\(.*\\) \\[via Relay\\]\" <noreply@relay.firefox.com>" mystr)
		  (string-match "<\\(.+\\)>" mystr))
	  (match-string-no-properties 1 mystr))))))

(defun jmm/notmuch-get-from-other-window ()
  "Same as `jmm/notmuch-get-from', just in the other window."
  (save-window-excursion
    (other-window 1)
    (jmm/notmuch-get-from)))

;; TODO: Also search for just the domain name.
;;;###autoload
(defun jmm/find-notmuch-rule ()
  (interactive)
  (let* ((fromaddr (jmm/notmuch-get-from-other-window))
	 (newpoint (save-excursion
		     (goto-char (point-min))
		     (when (re-search-forward (format "from:%s" (regexp-quote fromaddr)))
		       (point)))))
    (if newpoint
	(goto-char newpoint)
      (message "Can't find %s" fromaddr))))

;;;;;;;;;;
;; Date parsing
(setq nmd-months
      '((1 "January" "Jan")
	(2 "February" "Feb")
	(3 "March" "Mar")
	(4 "April" "Apr")
	(5 "May")
	(6 "June" "Jun")
	(7 "July" "Jul")
	(8 "August" "Aug")
	(9 "September" "Sep" "Sept")
	(10 "October" "Oct")
	(11 "November" "Nov")
	(12 "December" "Dec")))

(setq nmd-month-hash
      (let ((myhashtable (make-hash-table :test 'equal)))
	(cl-loop for monthlist in nmd-months
		 do (let ((main (car monthlist))
			  (other-abbrevs (cdr monthlist)))
		      (puthash (number-to-string main) main myhashtable)
		      (puthash (format "%02d" main) main myhashtable)
		      (cl-loop for x in monthlist
			       do (puthash (downcase x) main myhashtable))))
	myhashtable))

(defun nmd-days-zeropad ()
  `(or ,@(cl-loop for i from 1 to 31
		  append (list (format "%02d" i) (format "%d" i)))))

(rx-define nmd-monthname (eval `(or ,@(seq-filter #'stringp (flatten-list nmd-months)))))
(rx-define nmd-day (eval (nmd-days-zeropad)))

(rx-define nmd-year (eval `(or ,@(or (cl-loop for i from 1900 to 2100
					      collect (number-to-string i))))))

(rx-define nmd-endday (or eow "th" "st" "nth"))
(rx-define nmd-date1 (seq (group-n 1 nmd-monthname) (or " " "/") (group-n 2 nmd-day) nmd-endday))
(rx-define nmd-date2 (seq (group-n 1 (or "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12")) "/" (group-n 2 nmd-day) nmd-endday (opt "/" (group-n 3 nmd-year))))
(rx-define nmd-date3 (seq (group-n 2 nmd-day) nmd-endday (1+ whitespace) "of" (1+ whitespace) (group-n 1 nmd-monthname)))
(rx-define nmd-date4 (seq (group-n 2 nmd-day) nmd-endday (1+ whitespace) (group-n 1 nmd-monthname) (opt (1+ whitespace) (group-n 3 nmd-year))))

(rx-define nmd-date-any (or nmd-date1 nmd-date2 nmd-date3 nmd-date4))

(defun nmd-month-to-num (month)
  "Convert a MONTH like \"June\" to 6."
  (gethash (downcase month) nmd-month-hash))

;; FIXME: Doesn't work correctly yet.
(defun jmm/notmuch-get-email-date (search-terms)
  "Return a list of dates for SEARCH-TERMS.

SEARCH-TERMS should be a list, or it'll be converted to one.
"
  (let ((args '("show" "--format=sexp" "--body=false" "--format-version=4")))
    (thread-last (apply #'notmuch-call-notmuch-sexp (append args (if (listp search-terms)
							     search-terms
							     (list search-terms))))
		 (setq blah)
		 car ;; Not sure if this is correct
		 (seq-filter (lambda (x)
			       (thread-first (car x)
					     (plist-get :match))))
		 (mapcar (lambda (x) (thread-first (car x)
						   (plist-get :headers)
						   (plist-get :Date)))))))

(defun nmd-get-year ()
  "Get year for email at point."
  ;; This is pretty inefficient.
  (if-let* ((dates (jmm/notmuch-get-email-date (notmuch-search-find-thread-id))))
      (thread-last (parse-time-string (car dates))
		   encode-time
		   (format-time-string "%Y")
		   string-to-number)))

(defun nmd-find-date ()
  "Find a date in the current search buffer. Return an encoded time."
  (interactive nil notmuch-search-mode)
  (let (found)
    (while (and (not found)
		(re-search-forward (rx nmd-date-any) nil t))
      (and (memq 'notmuch-search-subject (get-text-property (point) 'face))
	   (setq found (encode-time `(0 0 0
					,(string-to-number (match-string-no-properties 2))
					,(nmd-month-to-num (match-string-no-properties 1))
					,(if-let ((year (match-string-no-properties 3)))
					     (string-to-number year)
					   (or (nmd-get-year) 2022))
					nil
					nil
					nil)))))
    found))

(defun nmd-mark-dates ()
  "Go through emails with dates, mark them as past or future."
  (interactive nil notmuch-search-mode)
  (let ((found t)
	test1)
    (while (setq found (nmd-find-date))
      (sit-for 0)
      (if (time-less-p found (current-time))
	  (notmuch-search-add-tag '("+past" "-future"))
	(notmuch-search-add-tag '("-past" "+future"))))))

;;;###autoload
(defun jmm/notmuch-add-to-promo ()
  "Add address to my bulk list."
  (interactive nil notmuch-search-mode)
  (let* ((changes (notmuch-search-interactive-tag-changes "+"))
	 (newtags (car changes))
	 (newtagsstr (mapconcat #'identity newtags " "))
	 (address (thread-first
		    (notmuch-search-find-stable-query)
		    car list
		    jmm/notmuch-get-email-addresses2
		    car))
	 firstpoint found execline)
    (save-window-excursion
      (notmuch-search-add-tag newtags)
      (bookmark-jump-other-window "pn-promo")
      (setq firstpoint (point))
      (goto-char (point-min))
      (if (re-search-forward (format "from:%s" (regexp-quote address)) nil t)
	  (progn
	    (setq found t)
	    (kill-new newtagsstr)
	    (recursive-edit))
	(progn
	  (goto-char firstpoint)
	  (forward-paragraph)
	  (insert (format "notmuch tag %s -- tag:new and from:%s" newtagsstr address))
	  (open-line 1)
	  (recursive-edit)
	  ))
      (setq execline (replace-regexp-in-string (regexp-quote "tag:new and ") "" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
    (when (yes-or-no-p (format "Run \"%s\": " execline))
      (shell-command execline)
      (notmuch-refresh-this-buffer))))

;;;###autoload
(defun jmm/notmuch-find-no-rule ()
  "Find next address without a rule in my post-new scripts."
  (interactive nil notmuch-search-mode)
  (let (found)
    (while (not found)
      (let* ((address (--> (notmuch-search-find-stable-query)
			   (list (car it))
			   (jmm/notmuch-get-email-addresses2 it)
			   (car it))))
	(save-window-excursion
	  (bookmark-jump-other-window "pn-promo")
	  (save-excursion
	    (goto-char (point-min))
	    (unless (re-search-forward (format "from:%s" (regexp-quote address)) nil t)
	      (message "No rule found for: %s" address)
	      (setq found t)))))
      (unless found
	(notmuch-search-next-thread)))))

(define-skeleton notmuch-skeleton-tags
  "Enter tags in any case and the output will be upcased."
  nil
  '(setq v1 (jmm/notmuch-get-from-other-window))
  "notmuch tag "
  ((skeleton-read "Tag: ") "" str " ")
  "-- tag:new and from:"
  v1 \n)

;;;###autoload
(defun jmm/notmuch-add-ledger-receipt ()
  "Add a receipt from the current email."
  (interactive nil notmuch-show-mode)
  (let* ((date (thread-last (notmuch-show-get-header :Date)
			    parse-time-string
			    encode-time
			    (format-time-string "%Y/%02m/%02d")))
	 (id (notmuch-show-get-message-id))
	 (link (org-store-link nil)))
    (bookmark-jump-other-window "l")
    (goto-char (point-min))
    (if (re-search-forward (regexp-quote id) nil t)
	(message "Receipt already found.")
      (progn
	(ledger-add-transaction (format "%s " date))
	(insert "* ")
	(save-excursion
	  (insert "\n" "    ; " link)
	  ;; (indent-region (line-beginning-position) (line-end-position))
	  )))))

(provide 'notmuch-stuff)
;; Local Variables:
;; read-symbol-shorthands: (("nmd-" . "jmm/notmuch-date-"))
;; End:
