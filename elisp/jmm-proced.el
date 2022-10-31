;;; jmm-proced.el --- Hide extraneous text in proced  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Joshua Moller-Mara

;; Author: Joshua Moller-Mara <jmm@cns.nyu.edu>
;; Keywords: processes

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

;; Utilities for `proced'.

;; Right now this just hides long /nix/store files paths, which can
;; clutter up my screen.
;; To use it, enable `jmm-proced-hide-details-mode'
;; or press "(" which is bound to `jmm-proced-hide-details-toggle-invisibility'.

;;; Code:

(defgroup jmm-proced nil
  "JMM Proced utilities"
  :group 'proced
  :prefix "jmm-proced-")

(defcustom jp-extra-paths-to-hide
  '("/run/current-system/sw/bin/")
  "Extra paths to make invisible.
Any path in this list gets replaced with invisible text."
  :type '(repeat string)
  :set (lambda (symbol value)
         (set-default symbol value)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (when (derived-mode-p 'proced-mode)
	       ;; Recompile keywords
	       (font-lock-add-keywords nil nil)
	       ;; Refresh
	       (font-lock-flush))))))

(defface jp-path-ignored
  '((t (:inherit shadow)))
  "Face used for paths ignored with `jmm-proced-hide-details-mode'.")

(defvar jp-path-invisible
  '(face jp-path-ignored invisible jp-path-detail)
  "Facespec for hiding paths")

(defvar jp-keywords
  `((,(rx (seq "/nix/store/" (1+ (any alnum "-" ".+")) "/" (0+ (seq (1+ alnum) "/"))))
     0 jp-path-invisible)
    (eval . (jp--generate-extra-path-keyword)))
  "`font-lock-keywords' to hide Nix store in proced")

(defun jp--generate-extra-path-keyword ()
  `(,(regexp-opt jp-extra-paths-to-hide) 0 jp-path-invisible))

;;;###autoload
(with-eval-after-load 'proced
  ;; TODO: Make it customizable whether we bind this key at all.
  (define-key proced-mode-map (kbd "(") #'jp-hide-details-toggle-invisibility))

;;;###autoload
(define-minor-mode jp-hide-details-mode
  "Testing out a minor mode for font locking"
  :lighter nil
  :interactive '(proced-mode)
  (unless (derived-mode-p 'proced-mode)
    (error "Not in proced mode"))
  (jp-hide-details-update-invisibility-spec)
  (if jp-hide-details-mode
      (progn
	(font-lock-add-keywords nil jp-keywords 'append)
	;; I'm using `font-lock-unfontify-region-function' instead of
	;; `font-lock-extra-managed-props' out of a paranoia of
	;; messing up other 'invisible properties set by others.
	;; That's also why I'm using advice to add the cleanup
	;; function instead of setting the variable outright.
	(add-function :before (local 'font-lock-unfontify-region-function)
		      #'jp--clean-up-invisible-properties)
	;; If it's not buffer-local, we can mess up other buffers like
	;; dired.
	;; (make-local-variable 'font-lock-extra-managed-props)
	;; (cl-pushnew 'invisible font-lock-extra-managed-props)
	)
    ;; Disable mode.
    (font-lock-remove-keywords nil jp-keywords)
    ;; Run `font-lock-unfontify-buffer' to strip 'invisible before
    ;; removing it from `font-lock-extra-managed-props'
    ;; (font-lock-unfontify-buffer)
    ;; Is it correct to remove 'invisible here?
    ;; What if we're not the only ones to set it?
    ;; (setf font-lock-extra-managed-props
    ;; 	  (seq-difference font-lock-extra-managed-props '(invisible)))
    )
  (font-lock-flush))

;;;###autoload
(defun jp-hide-details-toggle-invisibility ()
  "Toggle whether paths are invisible or just shadowed.
Turns on `jmm-proced-hide-details-mode' if it was not yet enabled."
  (interactive nil proced-mode)
  (if jp-hide-details-mode
      (funcall (if (member '(jp-path-detail . t) buffer-invisibility-spec)
		   'remove-from-invisibility-spec
		 'add-to-invisibility-spec)
	       '(jp-path-detail . t))
    (jp-hide-details-mode 1)))

(defun jp-hide-details-update-invisibility-spec ()
  (funcall (if jp-hide-details-mode
	       'add-to-invisibility-spec
	     'remove-from-invisibility-spec)
	   '(jp-path-detail . t)))

(defun jp--clean-up-invisible-properties (beg end)
  "Remove invisible properties we've set.
This gets added before `font-lock-unfontify-region-function'"
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char beg)
      (let ((inhibit-read-only t)
	    match)
	(while (setq match (text-property-search-forward
			    'invisible 'jp-path-detail
			    (lambda (val inspecting)
			      (memq val (ensure-list inspecting)))))
	  (if-let* ((newval (remq 'jp-path-detail (ensure-list (prop-match-value match)))))
	      (put-text-property (prop-match-beginning match)
				 (prop-match-end match)
				 'invisible
				 newval)
	    (remove-list-of-text-properties (prop-match-beginning match)
					    (prop-match-end match)
					    '(invisible))))))))

(provide 'jmm-proced)
;;; jmm-proced.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("jp-" . "jmm-proced-"))
;; End:
