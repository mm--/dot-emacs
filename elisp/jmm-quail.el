;;; jmm-quail.el --- Josh's extra input methods      -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Joshua Moller-Mara

;; Author: Joshua Moller-Mara <jmm@cns.nyu.edu>
;; Keywords: input method

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

;; These input methods make it easier to input some hard-to-type keys.

;; See
;; https://emacs.stackexchange.com/questions/58820/how-can-i-make-abbreviations-expand-instantly
;; for the original idea and
;; https://github.com/jorgenschaefer/typoel
;; for a similar package

;;; Code:

(require 'quail)

;; TODO: Would be handy to add marks, kind of like tempo marks.
(defun jmm-quail-move-point-backward-advice (str)
  "Looks at STR, checks the text property 'jmm-back and moves that many characters backward.
This is quail advice.
See `quail-define-rules' to see how advice works."
  (interactive)
  (when-let ((n (get-text-property 0 'jmm-back str)))
    (backward-char n)))

;;;###autoload (register-input-method "jmm-typography" "Latin-1" 'quail-use-package "“JT”" "Josh's typography input" "jmm-quail.el")

(quail-define-package
 "jmm-typography" "Latin-1" "“JT”" t
 "JMM's typography input method.
Makes it easier to do things like enter em dashes, curved quotes, ellipsis, and the like.
Also escapes some stuff for HTML.
" nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ((advice . jmm-quail-move-point-backward-advice))
 ("--" ?–)
 ("---" ?—)
 ("`" [?‘ ?`])
 ("'" [?’ ?'])
 ("``" ?“)
 ("''" ?”)
 ("..." ?…)
 ("\"\"" [#("“”" 0 2 (jmm-back 1))])
 ("&&" ["&amp;"])
 ("<<" ["&lt;"])
 (">>" ["&gt;"])
 ;; The "compose" input method is probably better here.
 ;; ("<-" ["<-" "←"])
 ;; ("->" ["->" "→"])
 )

(provide 'jmm-quail)
;;; jmm-quail.el ends here
