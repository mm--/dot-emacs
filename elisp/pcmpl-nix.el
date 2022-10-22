;;; pcmpl-nix.el --- Shell completions for Nix       -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Joshua Moller-Mara

;; Author: Joshua Moller-Mara <jmm@cns.nyu.edu>
;; Keywords: nix

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

;; Starting to provide some eshell/shell completion for Nix
;; This is still a work in progress

;;; Code:

;; This provides `jmm/nix-get-keys-of'
(require 'skeleton-test)

(require 'pcomplete)

(defun pcmpl-nix-get-keys-of (prefix attrset)
  "Get the keys of ATTRSET in nixpkgs.
Example: You can get a list of all rPackages."
  (let ((args (list "eval" "--raw"
		    (format "(with import <nixpkgs> {};
builtins.toJSON (builtins.filter (x: lib.strings.hasPrefix \"%s\" x) (builtins.attrNames %s)))" (or prefix "") attrset)
		    )))
    (json-parse-string
     (with-output-to-string
       (with-current-buffer standard-output
	 (apply #'call-process "nix" nil t nil args)))
     :array-type 'list)))

(defvar pcmpl-nix-commands
  '("run" "repl" "build" )
  "List of `nix' commands.
TODO: There's more, but this is just an example.")

(defvar pcmpl-nix-channels
  '("nixos-19.03" "nixos-19.09" "nixos-20.03" "nixos-21.11" "nixos-unstable")
  "List of `nix' channels.
Need to prepend \"channel:\" to it.
TODO: There's more, but this is just an example.")

;; TODO: Need to find a way to get attributes of a set, like "gst_all_1.gstreamer.dev"
;; TODO: Probably need to evaluate with the given channel. Like 18.03, etc.
;; TODO: This is actually pretty slow when completing the entire list of packages.
(defvar pcmpl-nix-lazy-nixpkgs
  (lazy-completion-table pcmpl-nix-lazy-nixpkgs
			 (lambda ()
			   (message "Calculating nixpkgs!")
			   (mapcar (lambda (x) (concat "nixpkgs." x)) (jmm/nix-get-keys-of "pkgs"))))
  "A lazy completion table of all nixpkgs, with \"nixpkgs.\" added.")

;;;###autoload
(defun pcomplete/nix ()
  "Completion for the \"nix\" command"
  (pcomplete-here* pcmpl-nix-commands)
  (cond
   ((pcomplete-test "run")
    (when (pcomplete-match "^-" 0)
      (pcomplete-opt "vlf")
      (cond
       ((pcomplete-test "-f")
	(pcomplete-here* (completion-table-with-cache
			  (lambda (_)
			    (mapcar (lambda (x) (concat "channel:" x)) pcmpl-nix-channels)))))))
    (pcomplete-here* pcmpl-nix-lazy-nixpkgs))
   ((pcomplete-test "repl")
    (when (pcomplete-match "^-" 0)
      (pcomplete-opt "I")
      (cond
       ((pcomplete-test "-I")
	(pcomplete-here (completion-table-with-cache
			 (lambda (_)
			   (mapcar (lambda (x) (concat "nixpkgs=channel:" x)) pcmpl-nix-channels)))
			nil nil t) ;; FORM-ONLY argument needed, otherwise won't complete when point is at "nixpkgs=^"
	)))
    ;; TODO: Look at other nix files, like default.nix
    ;; TODO: Quoting not working here?
    (pcomplete-here* (list "\"<nixpkgs>\"")))))

(provide 'pcmpl-nix)
;;; pcmpl-nix.el ends here
