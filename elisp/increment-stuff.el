;; -*- lexical-binding: t; -*-
(require 'rst)
(require 'thingatpt)

;; From https://www.emacswiki.org/emacs/IncrementNumber
;;;###autoload
(defun increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (let* ((e1 (save-excursion
	       (skip-chars-forward "0123456789")
	       (point-marker)))
	 (offset (- (point) e1))
	 inc-by field-width answer)
    (save-excursion
      (save-match-data
	(setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))
    (when answer
      (goto-char (+ e1 offset))
      (set-marker e1 nil))))

;; TODO: It'd be cool to handle roman numerals, "one", etc.
;; See https://emacs.stackexchange.com/questions/22743/in-emacs-lisp-how-to-convert-roman-numerals-to-an-integer
;; But we'd need to figure out how to not increment things like "i" as roman numeral unless we started with it.
;;;###autoload
(defun increment-number-roman (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let ((inc-by (if arg arg 1))
	    answer)
        (skip-chars-backward "IVXLCDMivxlcdm") ;; Not at all the right way
        (when (re-search-forward "[IVXLCDMivxlcdm]+" nil t)
          (setq answer (rst-arabic-to-roman (+ (rst-roman-to-arabic (match-string 0)) inc-by)))
	  (replace-match
	   (if (string-match-p "\\`[:upper:]+\\'" answer)
	       answer
	     (downcase answer))))))))

;; Adapted from https://emacs.stackexchange.com/questions/20481/incrementing-characters-in-emacs-next-to-numbers
;;;###autoload
(defun increment-number-or-char (&optional arg)
  "Increment the number or character forward from point by 'arg'."
  (interactive "p*")
  (if (save-excursion
	(skip-chars-backward "0123456789")
	(looking-at-p "[0-9]+"))
      (increment-number-decimal arg)
    (save-excursion
      (let* ((inc-by (if arg arg 1))
	     (chr (+ (char-after) inc-by)))
        (unless (characterp chr) (error "Cannot increment char by one"))
        (delete-char 1)
        (insert chr)))))

(defvar-keymap increment-number-or-char-repeat-map
  :doc "Keymap to repeat `increment-number-or-char' key sequences.  Used in `repeat-mode'."
  "i" (lambda ()
	(interactive)
	(setq repeat-map 'increment-number-or-char-repeat-map)
	(setq current-prefix-arg (prefix-numeric-value last-prefix-arg))
	(increment-number-or-char current-prefix-arg))
  "+" (lambda ()
	(interactive)
	(setq repeat-map 'increment-number-or-char-repeat-map)
	(setq current-prefix-arg (abs (prefix-numeric-value last-prefix-arg)))
	(increment-number-or-char current-prefix-arg))
  "=" (lambda ()
	(interactive)
	(setq repeat-map 'increment-number-or-char-repeat-map)
	(setq current-prefix-arg (abs (prefix-numeric-value last-prefix-arg)))
	(increment-number-or-char current-prefix-arg))
  "-" (lambda ()
	(interactive)
	(setq repeat-map 'increment-number-or-char-repeat-map)
	(setq current-prefix-arg (- (abs (prefix-numeric-value last-prefix-arg))))
	(increment-number-or-char current-prefix-arg)))
(put 'increment-number-or-char 'repeat-map 'increment-number-or-char-repeat-map)

;; Kind of like dlh-yank-increment from
;; https://www.emacswiki.org/emacs/IncrementNumber
;;;###autoload
(defun yank-increment-number-or-char (&optional arg)
  "Yank text, incrementing char or number."
  (interactive "p*")
  (let ((new-text (with-temp-buffer
		    (insert (current-kill 0))
		    (goto-char (point-min))
		    (increment-number-or-char arg)
		    (buffer-string))))
    (insert-for-yank new-text)
    (kill-new new-text t)))

;; This seems easier to use for macros
;;;###autoload
(defun yank-then-increment-number-or-char (&optional arg)
  "Yank text, then kill with incremented char or number."
  (interactive "p*")
  (let* ((old-text (current-kill 0))
	 (new-text (with-temp-buffer
		     (insert old-text)
		     (goto-char (point-min))
		     (increment-number-or-char arg)
		     (buffer-string))))
    (insert-for-yank old-text)
    (kill-new new-text t)))

;;; Incrementing dates

(rx-define ji--date-simple
  (seq (group-n 1 digit digit digit digit)
       "-"
       (group-n 2 digit digit)
       "-"
       (group-n 3 digit digit)))

(defun ji--date-zero-time (decodedtime)
  "Takes in a decoded time and sets nil hour/minute/seconds to 0."
  ;; (unless (decoded-time-minute decodedtime)
  ;;   (setf (decoded-time-minute decodedtime) 0))
  ;; (unless (decoded-time-hour decodedtime)
  ;;   (setf (decoded-time-hour decodedtime) 0))
  ;; (unless (decoded-time-second decodedtime)
  ;;   (setf (decoded-time-second decodedtime) 0))
  (seq-mapn (lambda (a b) (or a b))
	    decodedtime
	    '(0 0 0 nil nil nil nil nil nil)))

;;;###autoload
(defun ji-date (&optional days)
  "Increment the date at point by DAYS.
Days is in interactive prefix arg."
  (interactive "p*")
  (let ((inc-by (or days 1)))
    (if (thing-at-point-looking-at (rx ji--date-simple))
	(let* ((beg (match-beginning 0))
	       (end (match-end 0))
	       (date (ji--date-zero-time (iso8601-parse (match-string-no-properties 0))))
	       (newdate (decoded-time-add date (make-decoded-time :day inc-by))))
	  (replace-region-contents
	   beg end
	   (lambda () (format-time-string "%Y-%m-%d" (encode-time newdate)))))
      (user-error "No date at point"))))

;; FIXME: Reset forward-char if nothing found.
(defun ji--re-search-forward-and-move (regexp)
  "Run `re-search-forward', but make sure we move."
  (let ((start (point))
	repeat
	res)
    (while (and (= start (point))
		(progn
		  (when repeat (forward-char))
		  t)
		(setq res (re-search-forward regexp nil t))
		(goto-char (match-beginning 0)))
      (setq repeat t))
    res))

;;;###autoload
(defun ji-goto-next-date (reverse)
  "Find the next thing that looks like a date.
With interactive prefix REVERSE, search backwards."
  (interactive (list current-prefix-arg))
  ;; TODO: Should I also include the reverse?
  (if reverse
      (if (re-search-backward (rx ji--date-simple) nil t)
	  (goto-char (match-beginning 0))
	(user-error "No previous date found"))
    (unless (ji--re-search-forward-and-move (rx ji--date-simple))
      (user-error "No next date found"))))

(defvar-keymap ji-date-repeat-map
  :doc "Keymap to repeat `jmm-increment-date' key sequences.  Used in `repeat-mode'."
  "d" (lambda ()
	(interactive)
	(setq repeat-map 'ji-date-repeat-map)
	(setq current-prefix-arg (prefix-numeric-value last-prefix-arg))
	(ji-date current-prefix-arg))
  "+" (lambda ()
	(interactive)
	(setq repeat-map 'ji-date-repeat-map)
	(setq current-prefix-arg (abs (prefix-numeric-value last-prefix-arg)))
	(ji-date current-prefix-arg))
  "=" (lambda ()
	(interactive)
	(setq repeat-map 'ji-date-repeat-map)
	(setq current-prefix-arg (abs (prefix-numeric-value last-prefix-arg)))
	(ji-date current-prefix-arg))
  "-" (lambda ()
	(interactive)
	(setq repeat-map 'ji-date-repeat-map)
	(setq current-prefix-arg (- (abs (prefix-numeric-value last-prefix-arg))))
	(ji-date current-prefix-arg)))
(put 'ji-date 'repeat-map 'ji-date-repeat-map)

(defun ji--repeat-goto-next ()
  "Go to the next instance of whatever we were looking for last time.
Looks at `last-command' to repeat it."
  (interactive)
  (setq repeat-map (or (get last-command 'repeat-map)
		       'ji-goto-next-repeat-map))
  (setq current-prefix-arg nil)
  (setq this-command last-command)
  (funcall last-command current-prefix-arg))

(defun ji--repeat-goto-previous ()
  "Go to the previous instance of whatever we were looking for last time.
Looks at `last-command' to repeat it."
  (interactive)
  (setq repeat-map (or (get last-command 'repeat-map)
		       'ji-goto-next-repeat-map))
  (setq current-prefix-arg 1)
  (setq this-command last-command)
  (funcall last-command current-prefix-arg))

;; It seems like the repeat map needs to contain the same key used before.
(defvar-keymap ji-goto-next-repeat-map
  :doc "Keymap to repeat finding next thing to increment.  Used in `repeat-mode'."
  "n" #'ji--repeat-goto-next
  "p" #'ji--repeat-goto-previous)

(defvar-keymap ji-goto-next-date-repeat-map
  :doc "Keymap to repeat finding next date.  Used in `repeat-mode'."
  :parent ji-goto-next-repeat-map
  "d" #'ji--repeat-goto-next)
(put 'ji-goto-next-date 'repeat-map 'ji-goto-next-date-repeat-map)

;; (global-set-key (kbd "C-c i i") 'increment-number-or-char)
;; (global-set-key (kbd "C-c i r") 'increment-number-roman)
;; (global-set-key (kbd "C-c i C-y") 'yank-increment-number-or-char)
;; (global-set-key (kbd "C-c i y") 'yank-then-increment-number-or-char)
;; (global-set-key (kbd "C-c i d") 'ji-date)
;; (global-set-key (kbd "C-c i n d") 'ji-goto-next-date)

(provide 'increment-stuff)

;; Local Variables:
;; read-symbol-shorthands: (("ji-" . "jmm-increment-"))
;; End:
