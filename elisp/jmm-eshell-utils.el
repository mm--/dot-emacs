;;; jmm-eshell-utils.el --- Personal convenience functions for Eshell  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Joshua Moller-Mara

;; Author: Joshua Moller-Mara <jmm@cns.nyu.edu>
;; Keywords: convenience

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

;; A helpful variable to get a list of dired marked files:
;;  (setf (alist-get "dm" eshell-variable-aliases-list nil nil #'equal)
;;         (list 'jmm-eshell--dired-marked-relative nil t))

;; Make it easier to change extensions:
;;  (setf (alist-get ?X eshell-modifier-alist)
;;        '(jmm-eshell-pred-replace-extension))

;; Possible command:
;; "for x in $dm { echo $x; *cp $x $x(:X'.hello'); sleep 2 }"

;;; Code:
(require 'eshell)

;;;###autoload
(defun jmm-eshell-other-window (&optional arg)
  "Like `eshell', but prefer another window."
  (interactive "P")
  (let ((display-comint-buffer-action `(display-buffer-pop-up-window nil)))
    (eshell arg)))

;;;###autoload
(defun jmm-eshell-same-dir-other-window (&optional arg)
  "Like `jmm-eshell-other-window', but switch to current `default-directory'."
  (interactive "P")
  (let ((dir default-directory)
	(display-comint-buffer-action `(display-buffer-pop-up-window nil)))
    (eshell arg)
    (eshell/cd dir)
    (unless (eshell-interactive-process-p)
      (eshell-kill-input)
      (eshell-queue-input))))

(defun jmm-eshell--dired-window-marked-files (&optional localp)
  "Returns a list of marked files in all dired buffers with visible windows in the current frame.
See `dired-get-marked-files' for explanation of LOCALP."
  (cl-loop for w in (window-list)
	   for b = (window-buffer w)
	   if (provided-mode-derived-p (buffer-local-value 'major-mode b) 'dired-mode)
	   append (with-current-buffer b (dired-get-marked-files localp 'marked))))

;;;###autoload
(defun jmm-eshell-dired-marked-relative ()
  "Return list of dired marked files, relative to default-directory."
  (mapcar #'file-relative-name (jmm-eshell--dired-window-marked-files)))

;;;###autoload
(defun eshell/dmarked (&rest args)
  "Dired marked files.
Returns a list of dired marked files in dired buffers in visible windows for the current frame."
    (eshell-eval-using-options
     "dmrel" args
     '((?a "abs" nil abs "Output absolute paths instead of relative paths")
       (?h "help" nil nil "Output this help screen.")
       :preserve-args
       :usage "[-a]")
     (mapcar (if abs #'identity #'file-relative-name)
	     (jmm-eshell--dired-window-marked-files))))

;;;###autoload
(defun eshell/newext (ext &rest files)
  "Change extension of FILES to EXT."
  (mapcar
   (lambda (f)
     (format "%s%s" (file-name-sans-extension f) ext))
   (flatten-tree files)))

;;;###autoload
(defun eshell/dirof (arg &rest ignored)
  "Get the directory of some object."
  (cond
   ((bufferp arg) (buffer-local-value 'default-directory arg))
   ((stringp arg) (file-name-directory (expand-file-name arg)))
   ((listp arg) (apply #'eshell/dirof (flatten-tree arg)))))

;;;###autoload
(defun eshell/async-shell (&rest args)
  "Run `async-shell-command'."
  (async-shell-command (combine-and-quote-strings (flatten-tree args))))

;;;###autoload
(defun eshell/rg (&rest args)
  "Use Emacs grep facility with ripgrep."
  (eshell-grep "rg" (append (list "--no-heading" "--null" "--color=ansi" "--follow" "--no-messages" "--search-zip") args) t))

;;;###autoload
(defun jmm-eshell-pred-replace-extension (&optional repeat)
  "Return a modifier function that will change the extension of file names.
You don't need to include the initial dot.
So you could do $file(:X/wav/) to make a wav file.
"
  (let* ((newext (or (eshell-get-delimited-modifier-argument)
                     (error "Malformed extension modifier")))
         (function (lambda (str)
		     (format "%s.%s" (file-name-sans-extension str) newext))))
    (lambda (lst)
      (mapcar function lst))))

;;;###autoload
(defun jmm-eshell-insert-last-argument ()
  "Inserts argument from last evaluated line.
Sure, you could use \"$_\", but sometimes you like to see the argument."
  (interactive)
  ;; TODO: Make repeated invocations get last arguments of previous commands
  (insert (eshell-get-variable "_")))

(defvar-keymap jmm-eshell-minor-mode-map
  "C-c M-." #'jmm-eshell-insert-last-argument)

;;; Functions:

;;;###autoload
(define-minor-mode jmm-eshell-minor-mode
  "Josh's minor mode for eshell.

Mostly just for keybindings.

\\{jmm-eshell-minor-mode-map}"
  :keymap jmm-eshell-minor-mode-map)

(provide 'jmm-eshell-utils)
;;; jmm-eshell-utils.el ends here
