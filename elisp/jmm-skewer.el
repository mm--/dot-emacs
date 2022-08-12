;;; jmm-skewer.el --- Personal utilities for skewer  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Joshua Moller-Mara

;; Author: Joshua Moller-Mara <jmm@cns.nyu.edu>
;; Keywords: hypermedia, multimedia

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

;; Provides some simple utilities for skewer-mode <https://github.com/skeeto/skewer-mode>

;; For example:
;; 
;; - `jmm-skewer-reload-mode' will just reload an HTML page whenever
;;    you save the buffer, or press "C-c C-c"

;;; Code:

(require 'skewer-mode)
(require 'simple-httpd)

;;;###autoload
(defun jsk-reload ()
  "Reload an HTML page.
This will send a reload to any listening page with skewer."
  (interactive)
  (skewer-eval "window.location.reload();"))

(defun jsk--find-port ()
  "Find the port number to use, return as an integer if found."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (when (re-search-forward (rx "src=\"http://localhost:" (group (1+ digit)) "/skewer\"") nil t)
	(string-to-number (match-string 1))))))

(defun jsk--maybe-start ()
  "Ask to `run-skewer' "
  (let ((port (jsk--find-port)))
    (unless (and (httpd-running-p)
		 (or (not port) (= httpd-port port)))
      (when (yes-or-no-p (format "Start skewer on port %d?" port))
	(setq httpd-port port)
	(httpd-start)))))

(defun jsk--maybe-reload ()
  "Only reload if `jmm-skewer-reload-mode' is enabled."
  (when jsk-reload-mode
    (jsk-reload)))

(defvar-keymap jsk-reload-mode-map
  "C-c C-c" #'jsk-reload)

;;;###autoload
(define-minor-mode jmm-skewer-reload-mode
  "Add a keybinding to reload"
  :keymap jsk-reload-mode-map
  :lighter " jSRM"
  (if jsk-reload-mode
      (progn
	(jsk--maybe-start)
	(add-hook 'after-save-hook #'jsk--maybe-reload nil t))
    (remove-hook 'after-save-hook #'jsk---maybe-reload t)))

(provide 'jmm-skewer)
;;; jmm-skewer.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("jsk-" . "jmm-skewer-"))
;; End:
