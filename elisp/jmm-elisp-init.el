;;; jmm-elisp-init.el --- Initialize .emacs initialization  -*- lexical-binding: t; -*-

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

;; One issue with having all my elisp code in org is that all the
;; (require ...) statements get evaluated at init. This is the reason
;; I had to split out my org config into its own file, as it took too
;; long to load when I wasn't even using org.

;; The proper way to do things is to split code out into their own
;; packages and provide autoloads. To do this while keeping the org
;; babel stuff, we just supply a :tangle header argument, add
;; autoload comments, and make sure the (provide ...) matches the
;; tangled filename.

;;; Code:
(require 'org)

;;;###autoload
(defun jmm-elisp-recompile ()
  "Tangle, byte-compile, make autoloads."
  (interactive)
  (org-babel-tangle-file (locate-user-emacs-file "jmm-emacs.org") (locate-user-emacs-file "jmm-emacs.el"))
  (org-babel-tangle-file (locate-user-emacs-file "jmm-org-config.org") (locate-user-emacs-file "jmm-org-config.el"))
  ;; (byte-recompile-directory "~/.emacs.d" 0)
  (byte-recompile-directory (locate-user-emacs-file "split") 0)
  (make-directory-autoloads (locate-user-emacs-file "elisp/") (locate-user-emacs-file ".autoloads/jmmloaddefs.el"))
  (make-directory-autoloads (locate-user-emacs-file "split/") (locate-user-emacs-file ".autoloads/jmmloadsplit.el")))

(provide 'jmm-elisp-init)
;;; jmm-elisp-init.el ends here
