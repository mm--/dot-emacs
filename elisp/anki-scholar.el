;;; anki-scholar.el --- AnkiConnect cards with Semantic Scholar and Internet Archive  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Joshua Moller-Mara

;; Author: Joshua Moller-Mara <jmm@cns.nyu.edu>
;; Keywords: bib

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

;; Helps create Anki cards about papers using Semantic Scholar (or fatcat/Internet Archive Scholar).
;; This is pretty specific to me, as it requires a special Anki note type.

;;; Code:
(require 'rx)
(require 'dash)

(defun jmm-anki/url-get-json (url)
  (with-temp-buffer
    (url-insert-file-contents url)
    (json-parse-buffer :object-type 'alist)))

(defun jmm-scholar/get-info-for-doi (doi)
  "Take in a DOI and get information about the authors and such."
  (let ((doicleaned (->> doi
			 (replace-regexp-in-string (regexp-quote "https://doi.org/")
						   ""))))
    (jmm-anki/url-get-json (format "https://api.semanticscholar.org/graph/v1/paper/DOI:%s?fields=title,authors,year,venue"
				   (url-hexify-string doicleaned)))))

(rx-define jmm-anki/fatcat-url (seq "https://fatcat.wiki/release/" (group-n 1 (1+ alphanumeric))))

(defun jmm-fatcat/get-info-for-url (url)
  "Take in a fatcat URL and get information about the authors and such."
  (jmm-anki/url-get-json
   (save-match-data
     (when (string-match (rx jmm-anki/fatcat-url) url)
       (format "https://api.fatcat.wiki/v0/release/%s?expand=container,creators,files,filesets,webcaptures" (match-string-no-properties 1 url))))))

(defun jmm-fatcat/get-doi (jsonresult)
  "Return doi from fatcat as a string."
  (->> jsonresult
       (alist-get 'ext_ids)
       (alist-get 'doi)))

(defun jmm-fatcat/get-releaseid (jsonresult)
  "Return fatcat release id from fatcat as a string."
  (->> jsonresult
       (alist-get 'ident)
       (format "https://fatcat.wiki/release/%s")))

;; How to get a list of authors
(defun jmm-scholar/get-authors (jsonresult)o
  "Return authors as a string."
  (->> jsonresult
       (alist-get 'authors)
       (mapcar (lambda (x) (alist-get 'name x)))
       (-interpose ", ")
       (apply #'concat)))

(defun jmm-fatcat/get-authors (jsonresult)
  "Return authors from fatcat as a string."
  (->> jsonresult
       (alist-get 'contribs)
       (mapcar (lambda (x) (alist-get 'raw_name x)))
       (-interpose ", ")
       (apply #'concat)))

;; Get the year, as a string
(defun jmm-scholar/get-year (jsonresult)
  "Return article year as a string."
  (->> jsonresult
       (alist-get 'year)
       (number-to-string)))

(defun jmm-fatcat/get-year (jsonresult)
  "Return article year as a string."
  (->> jsonresult
       (alist-get 'release_year)
       (number-to-string)))

;; Get the journal
(defun jmm-scholar/get-journal (jsonresult)
  "Return the journal as a string."
  (->> jsonresult
       (alist-get 'venue)))

(defun jmm-fatcat/get-journal (jsonresult)
  "Return the journal as a string."
  (->> jsonresult
       (alist-get 'container)
       (alist-get 'name)))

;; Get the title
(defun jmm-scholar/get-title (jsonresult)
  "Get the article title as a string"
  (->> jsonresult
       (alist-get 'title)))

(defun jmm-fatcat/get-title (jsonresult)
  "Get the article title as a string"
  (->> jsonresult
       (alist-get 'title)))

(defun jmm-anki/anki-connect-query (query)
  (let ((url-request-method        "POST")
	(url-request-extra-headers `(("Content-Type" . "application/json")))
	(url-request-data           (encode-coding-string (json-serialize query) 'utf-8)))
    (with-temp-buffer
      (url-insert-file-contents "http://localhost:8765")
      (json-parse-buffer :object-type 'alist))))

;; (jmm-anki/anki-connect-query '((action . "deckNames")
;; 			       (version . 6)))

;;;###autoload
(defun jmm-anki/create-note-for-doi (doi)
  "Create an Anki card for a scientific paper, by looking up its DOI."
  (interactive "sDOI/URL: ")
  (let* ((jsonres (jmm-scholar/get-info-for-doi doi))
	 (mytitle (jmm-scholar/get-title jsonres))
	 (myauthor (jmm-scholar/get-authors jsonres))
	 (myyear (jmm-scholar/get-year jsonres))
	 (myjournal (jmm-scholar/get-journal jsonres)))
    (jmm-anki/anki-connect-query
		   `((action . "guiAddCards")
		     (version . 6)
		     (params . ((note . ((deckName . "Neuroscience")
					 (modelName . "Science2")
					 (fields . ((ArticleTitle . ,mytitle)
						    (Author . ,myauthor)
						    (Year . ,myyear)
						    (DOI . ,doi)
						    (Journal . ,myjournal)))))))))))

;;;###autoload
(defun jmm-anki/create-note-for-fatcat (fatcaturl)
  "Create an Anki card for a scientific paper, from its fatcat URL."
  (interactive "sURL: ")
  (let* ((jsonres (jmm-fatcat/get-info-for-url fatcaturl))
	 (mydoi (jmm-fatcat/get-doi jsonres))
	 (myreleaseid (jmm-fatcat/get-releaseid jsonres))
	 (mytitle (jmm-fatcat/get-title jsonres))
	 (myauthor (jmm-fatcat/get-authors jsonres))
	 (myyear (jmm-fatcat/get-year jsonres))
	 (myjournal (jmm-fatcat/get-journal jsonres)))
    (jmm-anki/anki-connect-query
		   `((action . "guiAddCards")
		     (version . 6)
		     (params . ((note . ((deckName . "Neuroscience")
					 (modelName . "Science2")
					 (fields . ((ArticleTitle . ,mytitle)
						    (Author . ,myauthor)
						    (Year . ,myyear)
						    (URL . ,myreleaseid)
						    (DOI . ,mydoi)
						    (Journal . ,myjournal)))))))))))

(defun jmm-anki/pdf-make-thumbnails (filename)
  "Makes a thumbnail view of a PDF."
  (interactive "fFilename: ")
  (let* ((default-directory "~/Downloads/tmp/")
	 (expanded-name (expand-file-name filename))
	 (newname (format "%s.png" (file-name-sans-extension (file-name-nondirectory filename)))))
    
    (shell-command (format "convert -thumbnail x150 -background white -alpha remove %s\\[0-4\\] jmm-anki-output.png"
			   (shell-quote-argument expanded-name)))
    (shell-command (format "montage jmm-anki-output-*.png -tile 5x -geometry +0+0 %s"
			   (shell-quote-argument newname)))
    (mapcar #'delete-file (file-expand-wildcards "jmm-anki-output*.png"))
    (let ((newfile (expand-file-name newname)))
      (kill-new newfile)
      (gui-set-selection 'PRIMARY newfile)
      (message "Copied: %s" newfile))))

(defun jmm-anki/download-pdf-make-thumbnails (url filename)
  "Downloads a PDF and makes a thumbnail view."
  (interactive "sURL: \nsFilename: ")
  (let* ((default-directory "~/Downloads/tmp/")
	 (_ (url-copy-file url filename)))
    (jmm-anki/pdf-make-thumbnails filename)))

;;; anki-scholar.el ends here
