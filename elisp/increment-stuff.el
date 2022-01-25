(require 'rst)

;; From https://www.emacswiki.org/emacs/IncrementNumber
;;;###autoload
(defun increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

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

;; (global-set-key (kbd "C-c i i") 'increment-number-or-char)
;; (global-set-key (kbd "C-c i r") 'increment-number-roman)
;; (global-set-key (kbd "C-c i C-y") 'yank-increment-number-or-char)
;; (global-set-key (kbd "C-c i y") 'yank-then-increment-number-or-char)

(provide 'increment-stuff)
