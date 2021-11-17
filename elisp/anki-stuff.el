(require 'face-remap) ;; Needed for adjusting text size

;;;###autoload
(defun jmm/anki-chinese ()
  "Set up buffers for quizzing Anki Chinese deck"
  (interactive)
  (delete-other-windows)
  (split-window-below)
  (split-window-below)
  (balance-windows)
  (switch-to-buffer "anki")
  (other-window 1)
  (switch-to-buffer "cangjie")
  (activate-input-method "Cangjie5")
  (setq text-scale-mode-amount 5)
  (text-scale-mode 1)
  (company-mode 0)
  (other-window 1)
  (switch-to-buffer "sisheng")
  (activate-input-method "chinese-sisheng")
  (setq text-scale-mode-amount 5)
  (text-scale-mode 1)
  (company-mode 0))

;;;###autoload
(defun jmm/anki-clojure ()
  "Set up buffers for quizzing Clojure cards"
  (interactive)
  (delete-other-windows)
  (find-file "~/code/clj/async-test/src/asyncstuff.clj")
  (unless (sesman-current-sessions 'CIDER '(project))
    (cider-jack-in nil))
  (cider-scratch))


(provide 'anki-stuff)
