;;; jmm-zotero.el --- Josh's Zotero/zotxt interface  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Joshua Moller-Mara

;; Author: Joshua Moller-Mara <jmm@cns.nyu.edu>
;; Keywords: wp, hypermedia

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

;; I'm writing a lot in HTML right now.  Unlike writing in LaTeX or
;; org-mode, there's not really a built-in way to get Zotero citations
;; into HTML. Well, you can copy citations and bibliographies from the
;; Edit menu, but I find this tedious, and it outputs citations in a
;; different format from what I'd like.

;; To make citations easier for me, I export the bibliography and add
;; the inline citation into a "data-" HTML attribute.  I then have
;; some Elisp that can prompt for a DOM ID and will insert the
;; associated "data-citation" and link to the bibliography.  It's kind
;; of hacky, but it's fine for now.

;; Another benefit is that I have a utility for easily opening a PDF
;; from a bibliography or citation.  When I alt-click on a
;; bibliography or citation, a Javascript utility looks at the
;; "data-view-url" to find the local PDF and opens it.

;; Note: Some of this is fragile for some reason.  Either there's a
;; race condition with fetching URLs from elisp, or there's a race
;; condition with how zotxt answers queries.

;;; Code:

(require 'plz)
(require 'dom)
(require 'rx)


;; Examples of how you'd use zotxt with plain curl:

;; curl "http://127.0.0.1:23119/zotxt/items?key=1_KMZMS8HK&format=bibliography"

;; curl -X POST http://127.0.0.1:23119/zotxt/bibliography
;;    -H 'Content-Type: application/json'
;;    -d '{"styleId":"chicago-author-date","citationGroups":[ { "citationItems": [{"key": "1_KMZMS8HK"}], "properties": {"noteIndex": 0}}]}'

;; curl -X POST http://127.0.0.1:23119/zotxt/bibliography -H 'Content-Type: application/json' -d '{"styleId":"apa","citationGroups":[ { "citationItems": [{"key": "1_KMZMS8HK"}], "properties": {"noteIndex": 0}}]}'

;; curl "http://127.0.0.1:23119/zotxt/items?key=1_KMZMS8HK&format=paths"

(defun jmm-zotero-get-bibliography (zoterokey)
  "Return a cons of the bibliography and citation for ZOTEROKEY."
  (pcase-let* ((res (plz 'post "http://127.0.0.1:23119/zotxt/bibliography"
		      :headers '(("Content-Type" . "application/json"))
		      :body (json-encode `(("styleId" . "apa")
					   ("citationGroups" . [ (("citationItems" . [ (("key" . ,zoterokey))]))])
					   ("properties" . (("noteIndex" . 0 )))))
		      :as #'json-read
		      :then 'sync))
	       ((map ('bibliography `[_ [,bibhtml]]) ('citationClusters `[,cite])) res)
	       (citestrip (thread-first cite
					(string-trim-left "(")
					(string-trim-right ")"))))
    (cons bibhtml citestrip)))

(defun jmm-zotero-get-attachment (zoterokey)
  "Get the path of an attachment for ZOTEROKEY.
Example key: 1_KMZMS8HK"
  (ignore-errors
    (pcase-let* ((res (plz 'get (format "http://127.0.0.1:23119/zotxt/items?key=%s&format=paths" zoterokey)
			:as #'json-read
			:then 'sync))
		 (`[,(map ('paths `[,path]))] res))
      path)))

(defun jmm-zotero-url-get-json (url)
  "Read JSON from URL.
This is currently only used to see if I'm getting a race
condition because of `plz' (which currently seems unlikely)."
  (with-temp-buffer
    (url-insert-file-contents url)
    (json-parse-buffer :object-type 'alist)))

(defun jmm-zotero-jmm-xhtml-bibliography-string (zoterokey)
  "Return an XHTML string for the bibliography for ZOTEROKEY."
  (pcase-let* ((attachment (jmm-zotero-get-attachment zoterokey))
	       ;; Sometimes zotxt returns nil for no reason?
	       ;; Too many requests at once?
	       ;; (_ (sit-for 0.5))
	       (`(,bibhtml . ,citestrip) (jmm-zotero-get-bibliography zoterokey))
	       (bibdomchildren (with-temp-buffer
				 (insert bibhtml)
				 (thread-first (libxml-parse-html-region (point-min) (point-max))
					       (dom-by-class "csl-entry")
					       car
					       dom-children)))
	       (id (thread-last citestrip
				 downcase
				 (replace-regexp-in-string (rx (1+ (not alphanumeric))) ""))))
    (with-temp-buffer
      (dom-print `(li ((id . ,id)
		       (class . "csl-entry")
		       (data-citation . ,citestrip)
		       ,@(when attachment `((data-view-url . ,(file-name-nondirectory attachment)))))
		      (span nil ,@bibdomchildren)))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun jmm-zotero-get-zoterokey-for-search (search-string)
  "Return a Zotero key given a SEARCH-STRING."
  (pcase-let ((`[,key] (plz 'get (format "http://127.0.0.1:23119/zotxt/search?q=%s&format=key"
					 (url-encode-url (replace-regexp-in-string " +" "+" search-string)))
			 :as #'json-read
			 :then 'sync)))
    key))


(defun jmm-zotero-get-zoterokey-for-search2 (search-string)
  "Return a Zotero key given a SEARCH-STRING."
  (let ((res (jmm-zotero-url-get-json (format "http://127.0.0.1:23119/zotxt/search?q=%s&format=key"
					      (url-encode-url (replace-regexp-in-string " +" "+" search-string))))))
    (pcase-let ((`[,key] res))
      key)))

(defun jmm-zotero-select-zoterokey-for-search (search-string)
  "Interactively select a Zotero key given a SEARCH-STRING."
  (let* ((keys (seq-into (plz 'get (format "http://127.0.0.1:23119/zotxt/search?q=%s&format=key"
					   (url-encode-url (replace-regexp-in-string " +" "+" search-string)))
			   :as #'json-read
			   :then 'sync)
			 'list))
	 (keyswithattachments (seq-filter (lambda (key)
					    (ignore-errors (jmm-zotero-get-attachment key)))
					  keys)))
    (if (length> keyswithattachments 1)
	(completing-read "Key: " keyswithattachments)
      (car keys))))

;; TODO: Mark interactive only since it might use completing-read
;;;###autoload
(defun jmm-zotero-insert-bibliography (search-string)
  "Insert bibliography information for SEARCH-STRING."
  (interactive (list (read-string "Zotero search string: ")) jmm-xhtml-mode)
  (let* ((key (jmm-zotero-select-zoterokey-for-search search-string))
	 ;; (_ (sit-for 1))
	 (xhtml (jmm-zotero-jmm-xhtml-bibliography-string key)))
    (insert xhtml)))

(provide 'jmm-zotero)
;;; jmm-zotero.el ends here
