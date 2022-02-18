;;; frame-local-theme-jmm.el --- Try loading themes in a frame-local manner  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Joshua Moller-Mara

;; Author: Joshua Moller-Mara <jmm@cns.nyu.edu>
;; Keywords: faces

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

;; Use `jmm/frame-local-load-theme' and `jmm/frame-local-reset-theme'
;; These are analogous to `load-theme' and `disable-theme'
;;
;; `jmm/frame-local-load-theme' doesn't yet take into account theme values.

;;; Code:

;;;###autoload
(defun jmm/frame-local-load-theme (theme)
  "Sets a theme just for the local frame.
A lot like `load-theme'.
NOTE: Undoing the settings doesn't work well, but see `jmm/frame-local-reset-theme'.
Also, I completely ignore theme-values."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar #'symbol-name
				       (custom-available-themes))))))
  (unless (custom-theme-name-valid-p theme)
    (error "Invalid theme name `%s'" theme))
  (load-theme theme nil t)
  (let ((settings (get theme 'theme-settings))
	(frame (selected-frame)))
    (dolist (s settings)
      (let* ((prop (car s))
	     (symbol (cadr s))
	     (localspec (cadddr s)))
	(cond
	 ((eq prop 'theme-face)
	  ;; TODO: Maybe store these specs so I can undo them?
	  (apply #'set-face-attribute symbol frame (face-spec-choose localspec frame))
	  ))))))

;;;###autoload
(defun jmm/frame-local-reset-theme ()
  "A bad way of undoing a frame-local theme.
It actually undoes themes in all frames, and I'm not yet sure why.
This is just a snippet from `disable-theme'."
  (interactive)
  (let (frame (selected-frame))
    (set-frame-parameter frame 'background-color
			 (custom--frame-color-default
			  frame :background "background" "Background"
			  "unspecified-bg" "white"))
    (set-frame-parameter frame 'foreground-color
			 (custom--frame-color-default
			  frame :foreground "foreground" "Foreground"
			  "unspecified-fg" "black"))
    (face-set-after-frame-default frame)))

(provide 'frame-local-theme-jmm)
;;; frame-local-theme-jmm.el ends here
