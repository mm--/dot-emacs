;;; anki-scholar.el --- AnkiConnect cards with Semantic Scholar  -*- lexical-binding: t; -*-

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

;; Helps create Anki cards about papers using Semantic Scholar.
;; This is pretty specific to me, as it requires a special Anki note type.

;;; Code:

(defun jmm-wiki/url-get-json (url)
  (with-temp-buffer
    (url-insert-file-contents url)
    (json-parse-buffer :object-type 'alist)))

(defun jmm-scholar/get-info-for-doi (doi)
  "Take in a DOI and get information about the authors and such."
  (let ((doicleaned (->> doi
			 (replace-regexp-in-string (regexp-quote "https://doi.org/")
						   ""))))
    (jmm-wiki/url-get-json (format "https://api.semanticscholar.org/graph/v1/paper/DOI:%s?fields=title,authors,year,venue"
				   (url-hexify-string doicleaned)))))

;; How to get a list of authors
(defun jmm-scholar/get-authors (jsonresult)
  "Return authors as a string."
  (->> jsonresult
       (alist-get 'authors)
       (mapcar (lambda (x) (alist-get 'name x)))
       (-interpose ", ")
       (apply #'concat)))

;; Get the year, as a string
(defun jmm-scholar/get-year (jsonresult)
  "Return article year as a string."
  (->> jsonresult
       (alist-get 'year)
       (number-to-string)))

;; Get the journal
(defun jmm-scholar/get-journal (jsonresult)
  "Return the journal as a string."
  (->> jsonresult
       (alist-get 'venue)))

;; Get the title
(defun jmm-scholar/get-title (jsonresult)
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

;;; anki-scholar.el ends here
