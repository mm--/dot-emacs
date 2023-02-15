;;; jmm-register.el --- Utilities for registers      -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Joshua Moller-Mara

;; Author: Joshua Moller-Mara <jmm@cns.nyu.edu>
;; Keywords: 

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

;; Provides two utilities to use registers with the kill ring.
;; `jmm-register-copy-kill-to-register' does kill => register
;; `jmm-register-register-to-kill-ring' does register => kill

;; Basic idea is that you often kill a lot of text.  Some text is
;; untentionally copied even though you'll never use it, like if you
;; use `zap-up-to-char'.  You don't want to `yank-pop' repeatedly to
;; find your important kills.  Saving the kill ring to a register lets
;; you effectively name important kills and keep them separate from
;; junk kills.

;; Also provides:
;; - `jmm-buffer-to-register' lets you save the buffer without the point
;; - `jmm-point-to-register' calls `jmm-buffer-to-register' if given a prefix

;;; Code:

(require 'register)

;;;###autoload
(defun jmm-register-copy-kill-to-register (register)
  "Copies the current kill to REGISTER.

Interactively, reads the register using `register-read-with-preview'."
  (interactive (list (register-read-with-preview "Current kill to register: ")))
  (set-register register (current-kill 0)))

;;;###autoload
(defun jmm-register-register-to-kill-ring (register)
  "Copies REGISTER to kill ring.

Interactively, reads the register using `register-read-with-preview'."
  (interactive (progn
		 (barf-if-buffer-read-only)
		 (list (register-read-with-preview "Copy register: "))))
  (let ((val (get-register register)))
    (jmm-register-val-kill-new val)))

(cl-defgeneric jmm-register-val-kill-new (_val)
  "Insert register value VAL."
  (user-error "Register does not contain text"))

(cl-defmethod jmm-register-val-kill-new ((val string))
  (kill-new val))

;;;###autoload
(defun jmm-buffer-to-register (register)
  "Sets current buffer to REGISTER.
Useful if you want to jump to a buffer, but don't want to save the point."
  (interactive (list (register-read-with-preview "Buffer to register: ")))
  (set-register register `(buffer . ,(current-buffer))))

;;;###autoload
(defun jmm-point-to-register (register &optional arg)
  "Like `point-to-register' but saves buffer if given a prefix.
See `jmm-buffer-to-register'."
  (interactive (list (register-read-with-preview
		      (if current-prefix-arg
			  "Buffer to register: "
			"Point to register: "))
		     current-prefix-arg))
  (if arg
      (jmm-buffer-to-register register)
    (point-to-register register)))

(provide 'jmm-register)
;;; jmm-register.el ends here
