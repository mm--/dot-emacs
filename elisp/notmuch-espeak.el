(defun jmm/notmuch-show-get-text ()
  "Return the \"text/plain\" part of the current message as a string."
  (save-excursion
    (goto-char (point-min))
    ;; I don't really know how to get a list of "parts", I don't even
    ;; know what the "part" type is. I just know that you can look for
    ;; changes in ":notmuch-part"
    (cl-loop
     do (goto-char (next-single-property-change (point) :notmuch-part))
     until (string-equal (plist-get (notmuch-show-get-part-properties) :content-type) "text/plain"))
    (let* ((msg (notmuch-show-get-message-properties))
	   (part (notmuch-show-get-part-properties)))
      (with-temp-buffer
	(notmuch-show-insert-part-text/plain msg part "text/plain" nil 0 nil)
	(buffer-substring-no-properties (point-min) (point-max))
	))))

;; TODO: Wash out URLs, so I don't espeak them.
;; TODO: Add some kind of hook to kill the process if the buffer is killed/buried

;;;###autoload
(defun jmm/notmuch-show-espeak (&optional arg)
  "Espeak the \"text/plain\" part of the current message, or kill it if it's running."
  (interactive "P")
  (let ((process-old (get-buffer-process " *espeak-notmuch*")))
    (if (process-live-p process-old)
	(kill-process process-old)
	(let* ((wpm (number-to-string (* (if arg arg 5) 100)))
               (process (start-process "espeak-process" " *espeak-notmuch*" "espeak" "-a" "200" "-s" wpm "--stdin")))
	  (process-send-string process (jmm/notmuch-show-get-text))
	  (process-send-string process "\n")
	  (process-send-eof process)))))

(provide 'notmuch-espeak)
