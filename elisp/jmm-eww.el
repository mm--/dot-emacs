;;; jmm-eww.el --- Josh's EWW utils                  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Joshua Moller-Mara

;; Author: Joshua Moller-Mara <jmm@cns.nyu.edu>
;; Keywords: hypermedia

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

;; `jmm-eww-links' generates a table of links, letting you see all URLs at once.

;; Maybe I should just make a "cybersurf" minor mode for eww.

;; To-dos/Ideas:
;; - [ ] Show only unique links or unique domain names
;; - [ ] Add filters like ibuffer.  For now just use `kill-matching-lines'.
;;       - [ ] Filter out mainstream sites like instagram, twitter, etc.
;; - [ ] Show only external links
;; - [ ] Remove javascript links
;; - [ ] Use the DOM to get "title=" attribute, when available.
;;     	 Possibly "alt" text for images.
;; - [ ] Remember "seen" links (possibly for filtering)
;;     	 Could probably use sqlite that comes with emacs now.
;; - [ ] Mark or filter dead links using DNS lookups
;; - [ ] Implement buffer undo or history
;;     	 (See `eww-save-history' for an example.)
;; - [ ] Make a command that tries to get the Wayback Machine version of a site,
;;       if it's down
;; - [ ] Make a `buffer-stale-function' for local files.
;;     	 Might also need to set `revert-buffer-function'

;;; Code:
(require 'vtable)

;; See https://lars.ingebrigtsen.no/2022/04/13/more-vtable-fun/

(defvar-keymap jmm-eww-link-table-mode-map
  :parent special-mode-map
  "n" #'jmm-eww-link-next-line
  "p" #'jmm-eww-link-previous-line
  "C-o" #'jmm-eww-display-link-position
  "o" #'jmm-eww-jump-link-position
  ;; TODO: Confusing mnemonics, need better names
  "j" #'jmm-eww-link-browse-url
  "C-j" #'jmm-eww-link-display-browse-url)

(define-derived-mode jmm-eww-link-table-mode special-mode "JMM Link Table"
  "Mode for listing links and their URLs.

\\{jmm-eww-link-table-mode-map}"
  (buffer-disable-undo)
  (setq-local revert-buffer-function #'jmm-eww--regenerate-link-table)
  (setq buffer-read-only t))

(defun jmm-eww--collect-links ()
  "Collect a list of URLs on current eww page.
Returns list of plists like ((:text \"some text\" :url \"http://example.com\")...)"
  (save-excursion
    (goto-char (point-min))
    (let ((win (selected-window))
	  (buf (current-buffer))
	  match)
      (thread-last
	(cl-loop while (setq match (text-property-search-forward 'shr-tab-stop nil nil t))
		 collect (progn
			   (goto-char (prop-match-beginning match))
			   (list
			    :text (thread-last
				    (buffer-substring-no-properties
				     (prop-match-beginning match)
				     (prop-match-end match))
				    (string-replace "\n" " " )
				    (string-trim))
			    :pos (point)
			    :buf buf
			    :win win
			    :url (get-text-property (point) 'shr-url))))
	(seq-filter (lambda (link) (plist-get link :url)))))))


(defun jmm-eww--follow-link (jmmlink)
  "Take in a link plist and browse to it.
Takes a plist from `jmm-eww--collect-links'.
The reason we use position POS instead of the URL is that
`eww-follow-link' transforms the URL and handles anchors specially.

However, it's possible that the POS may not be valid if the buffer has navigated elsewhere."
  (let ((buf (plist-get jmmlink :buf)))
    (if (buffer-live-p buf)
	(progn
	  (pop-to-buffer buf)
	  (goto-char (plist-get jmmlink :pos))
	  (eww-follow-link))
      (eww (plist-get jmmlink :url)))))

(defun jmm-eww-link-browse-url (&optional jmmlink)
  "Call `browse-url' for URL in table row.
Unlike `jmm-eww--follow-link', this directly navigates to the URL and
does not try to find the link in the original eww buffer."
  (interactive nil jmm-eww-link-table-mode)
  (let* ((jmmlink (or jmmlink (get-text-property (point) 'vtable-object)))
	 (buf (plist-get jmmlink :buf)))
    (when (buffer-live-p buf)
      (pop-to-buffer buf))
    (browse-url (plist-get jmmlink :url))))

(defun jmm-eww-link-display-browse-url ()
  "Like `jmm-eww-link-browse-url' but keeps current window selected."
  (interactive nil jmm-eww-link-table-mode)
  (save-selected-window
    (jmm-eww-link-browse-url)))

(defun jmm-eww-jump-link-position (&optional jmmlink)
  "Jump to position where link appears."
  (interactive nil jmm-eww-link-table-mode)
  (let* ((jmmlink (or jmmlink (get-text-property (point) 'vtable-object)))
	 (buf (plist-get jmmlink :buf)))
    (if (buffer-live-p buf)
	(progn
	  (pop-to-buffer buf)
	  (goto-char (plist-get jmmlink :pos)))
      (user-error "Original buffer was killed."))))

(defun jmm-eww-display-link-position (&optional jmmlink)
  "Display position where link appears."
  (interactive nil jmm-eww-link-table-mode)
  (save-selected-window
    (jmm-eww-jump-link-position jmmlink)))

(defun jmm-eww-link-next-line ()
  "Call `next-line' and go to position where link appears."
  (interactive nil jmm-eww-link-table-mode)
  (call-interactively #'next-line)
  (jmm-eww-display-link-position))

(defun jmm-eww-link-previous-line ()
  "Call `previous-line' and go to position where link appears."
  (interactive nil jmm-eww-link-table-mode)
  (call-interactively #'previous-line)
  (jmm-eww-display-link-position))

(defvar-local jmm-eww--link-original-buffer nil
  "Original eww buffer for our list of links.")

(defun jmm-eww--generate-link-table (buf ewwbuffer)
  (let ((links (with-current-buffer ewwbuffer
		 (jmm-eww--collect-links)))
	(inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      ;; This comes before making the table, otherwise we don't get
      ;; the table heading.
      (jmm-eww-link-table-mode)
      (setq-local jmm-eww--link-original-buffer ewwbuffer)
      (make-vtable
       :columns '((:name "Text" :width 20) "URL")
       :objects-function #'jmm-eww--recollect-links
       ;; You know, I'm not sure whether to define keys in :actions or as a mode keybinding.
       :actions '("RET" jmm-eww--follow-link)
       :getter (lambda (object column vtable)
		 (pcase (vtable-column vtable column)
		   ("Text" (plist-get object :text))
		   ("URL" (plist-get object :url))))))))

(defun jmm-eww--recollect-links ()
  "Regenerate links called by `vtable-revert'."
  (with-current-buffer jmm-eww--link-original-buffer
    (jmm-eww--collect-links)))

(defun jmm-eww--regenerate-link-table (ignore-auto noconfirm)
  "Regenerate link table possibly called by `revert-buffer'."
  (jmm-eww--generate-link-table (current-buffer) jmm-eww--link-original-buffer))

;;;###autoload
(defun jmm-eww-links ()
  "Display a table of links for the current EWW page."
  (interactive nil eww-mode)
  (let ((origbuf (current-buffer))
	(buf (get-buffer-create "*jmm-eww links*")))
    (jmm-eww--generate-link-table buf origbuf)
    (pop-to-buffer buf)))

(provide 'jmm-eww)
;;; jmm-eww.el ends here
