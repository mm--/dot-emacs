;;; jmm-notmuch-stuff.el --- JMM's utilities for notmuch    -*- lexical-binding: t; -*-

;; Copyright (C) 2021-2022  Joshua Moller-Mara

;; Author: Joshua Moller-Mara <jmm@cns.nyu.edu>
;; Keywords: mail, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(require 'xref)


;; Notmuch is great, but typing search queries like "date:2M.. and
;; tag:family" can be burdensome.  Saved searches help, but don't
;; solve the problem of one-off searches.
;;
;; What we can do though is use abbrevs!
;;
;; This adds advice that wraps around `notmuch-read-query' to
;; enable `abbrev-mode' and set a new abbreviation table.
;;
;; What you get is the ability to type complicated searches quickly.
;; The previous search becomes "d a tf".  Much faster!

(define-abbrev-table 'jmm-notmuch-search-abbrev-table
  (mapcar
   (lambda (x) (append x (list :system t)))
   '(("d2" "date:2M.." nil)
     ("dw" "date:\"this week\"" nil)
     ("dt" "date:today" nil)
     ("a" "and" nil)
     ("t" "tag:" jmm-no-self-insert)
     ("s" "subject:" jmm-no-self-insert)
     ("f" "from:" jmm-no-self-insert)
     ("tf" "tag:family" nil)
     ("tb" "tag:bulk" nil)
     ("ur" "tag:unread" nil)
     ("ev" "tag:events" nil)
     ))
  "Abbreviations for notmuch-search."
  :parents (list minibuffer-mode-abbrev-table)
  ;; TODO: Fix :regexp to ignore colons.
  )

(defun jmm-no-self-insert ()
  "Prevent adding last character when expanding abbrevs.
All this function has to do is return t to prevent insertion of a
space (or whichever character triggered expansion)."
  t)
(put 'jmm-no-self-insert 'no-self-insert t)

(defun jmm-notmuch-read-abbrevs (orig &rest args)
  "Advice to add `abbrev-mode' to `notmuch-read-query'.
Uses a separate abbrev table `jmm-notmuch-search-abbrev-table'.
ORIG is the original function, ARGS is the args passed to it."
  (minibuffer-with-setup-hook
      (:append (lambda ()
		 (setq-local local-abbrev-table jmm-notmuch-search-abbrev-table)
		 (abbrev-mode)))
    (apply orig args)))

(advice-add 'notmuch-read-query :around #'jmm-notmuch-read-abbrevs)


;;;###autoload
(defun jmm/notmuch-search-org ()
  "Find email ID in org mode.
Searches my org mode project for the notmuch ID, to see if I have
something already entered somewhere."
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
	  (user-error "No link found for this notmuch message ID"))))))

(fset 'jmm/notmuch-mark-all-previous-read
      (kmacro-lambda-form [?\C-e ?\C-  ?\C-a ?\M-< ?k ?U ?\C-x ?\C-x] 0 "%d"))

;; TODO: no-try-domain should be smart. Generic domains like
;; "gmail.com" or "nyu.edu" shouldn't be matched.

;;;###autoload
(defun jmm-notmuch-find-rule (fromaddr &optional no-try-domain no-error)
  "Find email address FROMADDR in current buffer.
When called interactively, gets FROMADDR from notmuch buffer in other window.

Will also try to just find the domain name if the address fails,
unless NO-TRY-DOMAIN is set.  Sometimes marketers change the
local-part they send from.

With NO-ERROR, don't signal an error if nothing is found, just
return nil."
  (interactive (list (jmm-notmuch-get-from-address-other-window) nil nil))
  (let* ((newpoint (save-excursion
		     (goto-char (point-min))
		     (when (or (re-search-forward
				(format "from:%s" (regexp-quote fromaddr))
				nil t)
			       ;; Try just the domain name
			       (unless no-try-domain
				 (re-search-forward
				  (format "from:%s"
					  (regexp-quote (cadr (split-string fromaddr "@"))))
				  nil t)))
		       (point)))))
    (if newpoint
	(goto-char newpoint)
      (unless no-error
	(user-error "Can't find %s" fromaddr)))))


;;;;;;;;;;
;; Date parsing
(defvar nmd-months
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

(defvar nmd-month-hash
  (let ((myhashtable (make-hash-table :test 'equal)))
    (cl-loop for monthlist in nmd-months
	     do (let ((main (car monthlist))
		      ;; (other-abbrevs (cdr monthlist))
		      )
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

(defun nmd--as-list (x)
  "If X isn't a list, embed it in a list."
  (if (listp x) x (list x)))

(defun nmd-find-date ()
  "Find a date in the current search buffer.
Return an encoded time."
  (interactive nil notmuch-search-mode)
  (let (found)
    (while (and (not found)
		(re-search-forward (rx nmd-date-any) nil t))
      (and (memq 'notmuch-search-subject (nmd--as-list (get-text-property (point) 'face)))
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

(defun jmm-notmuch-get-search-from-addresses (search-terms)
  "Get from addresses from a search.

Like `jmm/notmuch-get-email-addresses', but if the address is
from Firefox Relay, get the true email.

SEARCH-TERMS is a list of strings forming a notmuch search query."
  (cl-loop for addr1 in (apply #'notmuch-call-notmuch-sexp
			       "address" "--format=sexp" "--format-version=4"
			       search-terms)
	   collect (let* ((addr (plist-get addr1 :address))
			  (name (plist-get addr1 :name)))
		     (if (string-equal addr "replies@relay.firefox.com")
			 (save-match-data
			   (when (string-match "\\(.*\\) \\[via Relay\\]" name)
			     (match-string-no-properties 1 name)))
		       addr))))

(defun jmm-notmuch-get-from-address ()
  "Get email address from thread at point in current buffer."
  (when-let* ((x (cond
		  ((derived-mode-p 'notmuch-search-mode)
		   (car (notmuch-search-find-stable-query)))
		  ((derived-mode-p 'notmuch-show-mode)
		   (notmuch-show-get-message-id))
		  (t (error "Not sure how to get address for %s" major-mode)))))
    (thread-first
      x
      list
      jmm-notmuch-get-search-from-addresses
      car)))

(defun jmm-notmuch-get-from-address-other-window ()
  "Same as `jmm-notmuch-get-from-address', just in the other window."
  (save-window-excursion
    (other-window 1)
    (jmm-notmuch-get-from-address)))

;;;###autoload
(defun jmm-notmuch-search-from ()
  "Search for all messages from the email in the \"from\" field."
  (interactive nil notmuch-search-mode)
  (if-let* ((from (jmm-notmuch-get-from-address)))
      (progn
	(kill-new from)
	(notmuch-search (format "from:%s" from)))
    (user-error "No email address was found")))

;;;###autoload
(defun jmm-notmuch-search-limit-from-sender (&optional arg)
  "Limit a search to the email address under the point.
With optional prefix ARG, invert the search."
  (interactive "P" notmuch-search-mode)
  (if-let* ((from (jmm-notmuch-get-from-address)))
      (notmuch-search-filter (concat (when arg "not ") (format "from:%s" from)))
    (user-error "No email address was found")))

;;;###autoload
(defun jmm-notmuch-find-sender-rule (&optional try-domain)
  "Find email tagging rule for sender.
Assumes you're in a notmuch search buffer.
With interactive arg TRY-DOMAIN, attempt to search for the domain if
searching for the local-part fails."
  (interactive "P" notmuch-search-mode notmuch-show-mode)
  (let* ((address (jmm-notmuch-get-from-address))
	 found start end execline)
    (save-current-buffer
      (bookmark-jump "post-new" #'ignore)
      (when (jmm-notmuch-find-rule address (not try-domain) t)
	(setq found (point-marker)
	      start (match-beginning 0)
	      end   (match-end 0))))
    (if found
      (prog1
	(when (pop-to-buffer (marker-buffer found))
	  ;; I think I'm supposed to double check that pop-to-buffer worked
	  (prog1
	      (goto-char found)
	    (pulse-momentary-highlight-region start end)))
	(set-marker found nil))
      (user-error "Can't find address %s" address))))

(defun jmm-notmuch--tag-changes ()
  "Read and execute tag changes.
Returns a list of changed tags."
  (cond
   ((derived-mode-p 'notmuch-search-mode)
    (let ((changes (car (notmuch-search-interactive-tag-changes "+"))))
      (notmuch-search-add-tag changes)
      changes))
   ((derived-mode-p 'notmuch-show-mode)
    (let ((changes (notmuch-read-tag-changes (notmuch-show-get-tags)
					     "Tag message" "+")))
      (notmuch-show-tag changes)
      changes))
   (t (error "Not sure how to add tags for %s" major-mode))))

;;;###autoload
(defun jmm-notmuch-add-to-promo (&optional try-domain)
  "Add address to my bulk list.
With prefix arg TRY-DOMAIN, fall back to domain name if local part not found."
  (interactive "P" notmuch-search-mode)
  (let* ((newtags (jmm-notmuch--tag-changes))
	 (newtagsstr (mapconcat #'identity newtags " "))
	 (address (jmm-notmuch-get-from-address))
	 execline)
    (save-window-excursion
      (bookmark-jump-other-window "post-new promotions")
      (if (jmm-notmuch-find-rule address (not try-domain) t)
	  (progn
	    ;; Make it easy to add new tags
	    (kill-new newtagsstr)
	    (recursive-edit))
	(forward-paragraph)
	(insert (format "notmuch tag %s -- tag:new and from:%s" newtagsstr address))
	(open-line 1)
	(recursive-edit))
      (jmm/notmuch-post-new-rerun-rule))))

;;;###autoload
(defun jmm-notmuch-post-new-rerun-rule ()
  "Re-run post-new rule, removing \"tag:new\"."
  (interactive)
  (let ((execline (replace-regexp-in-string (regexp-quote "tag:new and ") "" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
    (when (y-or-n-p (format "Run \"%s\"?" execline))
      (shell-command execline)
      ;; Refresh notmuch window.
      (cl-loop for w in (window-list)
	       for b = (window-buffer w)
	       if (provided-mode-derived-p (buffer-local-value 'major-mode b) 'notmuch-search-mode)
	       do (with-current-buffer b (notmuch-refresh-this-buffer))))))

;;;###autoload
(defun jmm-notmuch-find-no-from-rule ()
  "Find next from address without a rule in my post-new scripts."
  (interactive nil notmuch-search-mode)
  (let ((buf (save-current-buffer
	       (bookmark-jump "post-new" #'ignore)
	       (current-buffer))))
    (catch 'found
      (while (and
	      (when-let* ((address (jmm-notmuch-get-from-address)))
		(with-current-buffer buf
		  (if (jmm-notmuch-find-rule address nil t)
		      t
		    (message "No rule found for: %s" address)
		    nil)))
	      (notmuch-search-next-thread))))))

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

(provide 'jmm-notmuch-stuff)
;;; jmm-notmuch-stuff.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("nmd-" . "jmm/notmuch-date-"))
;; End:
