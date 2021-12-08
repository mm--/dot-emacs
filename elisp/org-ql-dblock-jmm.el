(require 'org)
(require 'org-ql)

(defun jmm/org-timestamp-list ()
  "Return a list of timestamps for the current entry"
  (let ((limit (org-entry-end-position))
	(regexp org-tsr-regexp))
    (cl-macrolet ((next-timestamp ()
				  `(when (re-search-forward regexp limit t)
				     (match-string-no-properties 0))))
      (save-excursion
	(cl-loop for next-ts = (next-timestamp)
		 while next-ts
		 collect next-ts)))))

;; FIXME: I don't know if this widens things properly
(defun jmm-tsa-element (element)
  (let ((orgmarker (org-element-property :org-hd-marker element)))
    (with-current-buffer (marker-buffer orgmarker)
      (save-excursion
	(goto-char (marker-position orgmarker))
	(string-join (sort (jmm/org-timestamp-list) #'string<) ",")))))

;; This is copied and modified from org-ql-search
;;;###autoload
(cl-defun org-dblock-write:org-ql-jmm-events (params)
  "Insert content for org-ql dynamic block at point according to PARAMS.
Valid parameters include:

  :query    An Org QL query expression in either sexp or string
            form.

  :columns  A list of columns, including `heading', `todo',
            `property', `priority', `deadline', `scheduled'.
            Each column may also be specified as a list with the
            second element being a header string.  For example,
            to abbreviate the priority column: (priority \"P\").
            For certain columns, like `property', arguments may
            be passed by specifying the column type itself as a
            list.  For example, to display a column showing the
            values of a property named \"milestone\", with the
            header being abbreviated to \"M\":

              ((property \"milestone\") \"M\").

  :sort     One or a list of Org QL sorting methods
            (see `org-ql-select').

  :take     Optionally take a number of results from the front (a
            positive number) or the end (a negative number) of
            the results.

  :ts-format  Optional format string used to format
              timestamp-based columns.

For example, an org-ql dynamic block header could look like:

  #+BEGIN: org-ql :query (todo \"UNDERWAY\") :columns (priority todo heading) :sort (priority date) :ts-format \"%Y-%m-%d %H:%M\""
  (-let* (((&plist :query :columns :sort :ts-format :take) params)
          (query (cl-etypecase query
                   (string (org-ql--query-string-to-sexp query))
                   (list  ;; SAFETY: Query is in sexp form: ask for confirmation, because it could contain arbitrary code.
                    (org-ql--ask-unsafe-query query)
                    query)))
          (columns (or columns '(heading todo (priority "P"))))
          ;; MAYBE: Custom column functions.
          (format-fns
           ;; NOTE: Backquoting this alist prevents the lambdas from seeing
           ;; the variable `ts-format', so we use `list' and `cons'.
           (list (cons 'todo (lambda (element)
                               (org-element-property :todo-keyword element)))
                 (cons 'heading (lambda (element)
                                  (org-make-link-string (org-element-property :raw-value element)
                                                        (org-link-display-format
                                                         (org-element-property :raw-value element)))))
                 (cons 'priority (lambda (element)
                                   (--when-let (org-element-property :priority element)
                                     (char-to-string it))))
                 (cons 'deadline (lambda (element)
                                   (--when-let (org-element-property :deadline element)
                                     (ts-format ts-format (ts-parse-org-element it)))))
		 (cons 'tsa (lambda (element)
			      (jmm-tsa-element element)))
                 (cons 'scheduled (lambda (element)
                                    (--when-let (org-element-property :scheduled element)
                                      (ts-format ts-format (ts-parse-org-element it)))))
                 (cons 'property (lambda (element property)
                                   (org-element-property (intern (concat ":" (upcase property))) element)))))
          (elements (org-ql-query :from (current-buffer)
                                  :where query
                                  ;; :select '(org-element-headline-parser (line-end-position))
				  :select 'element-with-markers
                                  :order-by sort)))
    (cl-labels ((format-element
                 (element) (string-join (cl-loop for column in columns
                                                 collect (or (pcase-exhaustive column
                                                               ((pred symbolp)
                                                                (funcall (alist-get column format-fns) element))
                                                               (`((,column . ,args) ,_header)
                                                                (apply (alist-get column format-fns) element args))
                                                               (`(,column ,_header)
                                                                (funcall (alist-get column format-fns) element)))
                                                             ""))
                                        " | ")))
      ;; Table header
      (insert "| " (string-join (--map (pcase it
                                         ((pred symbolp) (capitalize (symbol-name it)))
                                         (`(,_ ,name) name))
                                       columns)
                                " | ")
              " |" "\n")
      (insert "|- \n")  ; Separator hline
      (dolist (element elements)
        (insert "| " (format-element element) " |" "\n"))
      (delete-char -1)
      (org-table-align)
      (org-table-analyze) ;; Needed for org-table-goto-field
      (org-table-goto-field "@2$2") ;; Sort by timestamps
      (org-table-sort-lines nil ?a))))

(provide 'org-ql-dblock-jmm)
