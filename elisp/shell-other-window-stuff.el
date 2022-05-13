;; TODO: Probably make buffer names based on default-directory, not
;; the current buffer name.

;;;###autoload
(defun jmm/ansi-term-other-window ()
  "Like `ansi-term', but show in another window."
  (interactive)
  (let ((ansi-term-buffer (save-window-excursion
			    (ansi-term shell-file-name (concat "ansi-term " (buffer-name))))))
    (pop-to-buffer ansi-term-buffer)))

;;;###autoload
(defun jmm/eshell-other-window ()
  "Like `project-eshell', but without a project and show in another window."
  (interactive)
  (defvar eshell-buffer-name)
  (let* ((eshell-buffer-name (format "*eshell-%s*" (buffer-name)))
         (eshell-buffer (get-buffer eshell-buffer-name)))
    (if (and eshell-buffer (not current-prefix-arg))
        (pop-to-buffer eshell-buffer)
      (pop-to-buffer (save-window-excursion (eshell t))))))

;;;###autoload
(defun jmm/shell-other-window ()
  "Like `project-shell', but without a project and show in another window."
  (interactive)
  (let* ((shell-buffer-name (format "*shell-%s*" (buffer-name)))
         (shell-buffer (get-buffer shell-buffer-name)))
    (if (and shell-buffer (not current-prefix-arg))
        (pop-to-buffer shell-buffer)
      (pop-to-buffer (save-window-excursion (shell shell-buffer-name))))))

(provide 'shell-other-window-stuff)
