;;; boilerplate-jmm.el --- Personal boilerplate for different programming languages  -*- lexical-binding: t; -*-

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

;; This is for boilerplate for things like Nix setups for R, or
;; setting up ClojureScript projects.

;;; Code:

(defmacro boilerplate-save-excursion (&rest body)
  "Run BODY, saving window configuration, entering a recursive edit, and then reverting and redisplaying the starting window (usually a dired buffer)."
  (declare (indent 0) (debug t))
  `(progn
     (save-window-excursion
       (progn ,@body)
       (recursive-edit))
     (revert-buffer)
     (redisplay)))

;;;###autoload
(defun boilerplate-r-nix ()
  "In the current directory, set up an R project with Nix and direnv.
Run this in a dired buffer of the new project."
  (interactive)
  (let ((mydir default-directory)
	mainfile mygit)
    (boilerplate-save-excursion
      (find-file (concat mydir ".envrc"))
      (set-buffer-modified-p t))
    (when (y-or-n-p "Trust env?")
      (shell-command (concat "direnv allow " mydir))
      (envrc-reload))
    (boilerplate-save-excursion
      (find-file (concat mydir "default.nix"))
      (nix-default-r-skeleton))
    (when (y-or-n-p "Build env?")
      (boilerplate-save-excursion
	(async-shell-command "nix build -vL -f . myenv -o myenv")))
    (setq mainfile (read-file-name "Main file: " default-directory))
    (when (setq mygit (y-or-n-p "Make git?"))
      (shell-command "git init")
      (boilerplate-save-excursion
	(find-file (concat mydir ".gitignore"))
	(insert "myenv\n"))
      (shell-command "git add .gitignore .envrc default.nix"))
    (boilerplate-save-excursion
      (find-file mainfile)
      (when mygit
	(set-buffer-modified-p t)
	(save-buffer)
	(shell-command (concat "git add " mainfile)))
      (ess-switch-process))
    (when mygit
      (magit-status))))


(provide 'boilerplate-jmm)
;;; boilerplate-jmm.el ends here
