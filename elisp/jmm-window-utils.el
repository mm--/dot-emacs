;;; jmm-window-utils.el --- Josh's emacs window utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Joshua Moller-Mara

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

;; Utilities for managing other windows.
;; - `jmm-quit-other-window' is handy when a window pops up that you'd like to quit.
;;   By default I bind it to "C-x 4 q"
;; - `jmm-other-window-buffer-here' move the buffer in the other window here.
;;    Useful if a buffer unexpectedly pops up in another window.

;;; Code:

;;;###autoload
(defun jmm-quit-other-window ()
  "Go to other window and quit it.
Should restore previous layout, but has slightly different semantics
than `winner-undo'."
  (interactive)
  (save-selected-window
    (other-window 1)
    (quit-window)))

;; (bind-key "C-x 4 q" #'jmm-quit-other-window)

;;;###autoload
(defun jmm-other-window-buffer-here ()
  "Move buffer in other window to this window.
Use this when a buffer pops up in another window, but you actually wanted it in this window."
  (interactive)
  (let (buf)
    (save-selected-window
      (other-window 1)
      (setq buf (current-buffer))
      (quit-window))
    (pop-to-buffer-same-window buf)))

(provide 'jmm-window-utils)
;;; jmm-window-utils.el ends here
