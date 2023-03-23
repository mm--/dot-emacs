;;; jmm-inkscape.el --- Control Inkscape through D-Bus  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Joshua Moller-Mara

;; Author: Joshua Moller-Mara <jmm@cns.nyu.edu>
;; Keywords: multimedia

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

;; This code makes it easier to go back and forth between editing SVG
;; files in Inkscape.

;; To send commands, it uses the D-Bus interface.  Specifically, it
;; uses the newer Gtk Actions interface from Inkscape 1.2.
;; Some info about that is at the Inkscape Wiki:
;; https://wiki.inkscape.org/wiki/DBus

;; Commands of interest:
;; - `jmm-inkscape-revert' tells Inkscape to revert a document.
;; - `jmm-inkscape-goto-selected-node' goes to the selected nodes.

;; How to use this:

;; 1. When using `jmm-inkscape-svg-mode' to edit SVG files,  "C-c C-j C-l"
;;    (`jmm-inkscape-launch') opens an Inkscape subprocess and remembers
;;    which Inkscape window is associated with your buffer.  The
;;    reason we don't use an already-running Inkscape process is
;;    because Inkscape currently only communicates results through
;;    standard output.  Weird, right? If you query it *through D-Bus*
;;    it will print the answer on *standard out*, instead of, you know,
;;    talking to you on the line you're talking on.
;;
;; 2. If you have something selected in Inkscape, "M-g s"
;;    (`jmm-inkscape-goto-selected-node') will jump to the first
;;    selected object.  Use the built-in `next-error' (default "M-g n") to
;;    jump to the next selected object.
;;
;; 3. Turn on the minor mode `jmm-inkscape-auto-reload-mode'.  Now
;;     when you save the buffer, it will automatically call Inkscape's
;;     "File > Revert" action to view the updated contents.


;; Some configuration you might use:
;; (setq auto-mode-alist
;;       (append
;;        '(("\\.svgz?\\'" . image-mode)
;; 	 ("\\.svgz?\\'" . jmm-inkscape-svg-mode))
;;        auto-mode-alist))

;; To-dos:
;; - [ ] We should probably run Inkscape with a specific D-Bus name,
;;       that way we don't have to guess which window to use.
;; - [ ] Clean up when the Inkscape window associated with our buffer
;;       closes, that way we don't keep trying to send it stuff.
;; - [ ] Select objects in Inkscape from Emacs.  Add/remove selections.
;; - [ ] Utilities for reformatting Inkscape's XML output, which can
;;       be hard to read if xml:space="preserve".
;; - [ ] (Personal) I don't really grasp D-Bus type signatures yet, or
;;       more specifically how to pass empty, type-conforming
;;       parameters to methods.  I figured some of these out through
;;       trial-and-error in elisp, but I don't know how to do the same
;;       thing in busctl or D-feet.
;; - [X] A command to make a rectangle the size of the viewport.
;;     	 Makes it easier to resize the document in Inkscape using
;;     	 “Resize Page to Selection”.
;; - [ ] Be able to change "transform" translations to x,y attributes, and
;;       vice versa
;; - [ ] Turn objects into clones, using bounding boxes to correctly position them

;;; Code:
(require 'align)
(require 'dbus)
(require 'jmm-nxml)
(require 'dom)

(defvar-local jmm-inkscape-window-id nil
  "The Inkscape document number for this buffer.
A number.")

(defvar-local jmm-inkscape-orig-buffer nil
  "Original buffer used for an ID search.")

(defvar jmm-inkscape-process nil
  "Holds a reference to the main Inkscape process.
See `jmm-inkscape-launch'.")

(defvar jmm-inkscape-last-selection nil
  "A list of selection IDs.
See `jmm-inkscape-refresh-selection'.")

(defun jmm-inkscape-revert ()
  "Tell Inkscape to revert the document.
This is the same as going to File > Revert.
You'll only want to do this if you've made changes from Emacs that
need to be reflected in Inkscape."
  (interactive)
  ;; I have no idea how to actually use org.gtk.Actions
  ;; This just seemed to work
  (dbus-call-method :session "org.inkscape.Inkscape"
		    (format "/org/inkscape/Inkscape/window/%s" jmm-inkscape-window-id)
		    "org.gtk.Actions" "Activate"
		    "document-revert"
		    '(:array :signature "v")
		    '(:array :signature "{sv}"))
  ;; I assume if it doesn't error that it succeeded
  (message "Inkscape reverted window %s" jmm-inkscape-window-id))

(defun jmm-inkscape-select-by-id (id)
  "Selects an ID in Inkscape.
ID is a string."
  (dbus-call-method :session "org.inkscape.Inkscape"
		    "/org/inkscape/Inkscape"
		    "org.gtk.Actions" "Activate"
		    "select-by-id"
		    `(:array (:variant ,id))
		    '(:array :signature "{sv}")))

(defun jmm-inkscape--read-selection-register (prompt)
  "Read a key from 1 to 9."
  (ignore-errors (string-to-number (char-to-string (read-key prompt)))))

(defun jmm-inkscape-selection-refresh ()
  "Store selection in `jmm-inkscape-last-selection'."
  (interactive nil jmm-inkscape-svg-mode)
  (setq jmm-inkscape-last-selection (jmm-inkscape-get-selection-ids))
  (message "Selected IDs: %S" jmm-inkscape-last-selection))

(defun jmm-inkscape-selection-jump-to-register (n)
  "Go to element N of `jmm-inkscape-last-selection'.
Starts at 1."
  (interactive (list (jmm-inkscape--read-selection-register "Selection: "))
	       jmm-inkscape-svg-mode)
  (if-let* ((id (elt jmm-inkscape-last-selection (1- n))))
      (jmm-inkscape--goto-id id)
    (error "No valid ID")))

(defun jmm-inkscape-selection-insert-register (n)
  "Insert ID of element N of `jmm-inkscape-last-selection'.
Starts at 1.
Includes a #."
  (interactive (list (jmm-inkscape--read-selection-register "Selection: "))
	       jmm-inkscape-svg-mode)
  (if-let* ((id (elt jmm-inkscape-last-selection (1- n))))
      (insert "#" id)
    (error "No valid ID")))

(defun jmm-inkscape-selection-add-node ()
  "Select the node at point."
  (interactive nil jmm-inkscape-svg-mode)
  (let ((id (save-excursion
	      (jnx--element-for-point)
	      (jnx--attribute-value "id"))))
    (jmm-inkscape-select-by-id id)))

;; (dbus-call-method :session "org.inkscape.Inkscape"
;; 		    "/org/inkscape/Inkscape"
;; 		    "org.gtk.Actions" "Activate"
;; 		    "select-clear"
;; 		    '(:array :signature "v")
;; 		    '(:array :signature "{sv}"))

;; Check out share/keys/inkscape.xml in the Inkscape source to see
;; examples of actions.
;; (dbus-call-method :session "org.inkscape.Inkscape"
;; 		  (format "/org/inkscape/Inkscape/window/%s" jmm-inkscape-window-id)
;; 		  "org.gtk.Actions" "Activate"
;; 		  "dialog-open"
;; 		  '(:array (:variant "Swatches"))
;; 		  '(:array :signature "{sv}"))

;; (progn
;;   (dbus-call-method :session "org.inkscape.Inkscape"
;; 		    (format "/org/inkscape/Inkscape/window/%s" jmm-inkscape-window-id)
;; 		    "org.gtk.Actions" "Activate"
;; 		    "node-align-horizontal"
;; 		    '(:array (:variant "last"))
;; 		    '(:array :signature "{sv}"))
;;   (dbus-call-method :session "org.inkscape.Inkscape"
;; 		    (format "/org/inkscape/Inkscape/window/%s" jmm-inkscape-window-id)
;; 		    "org.gtk.Actions" "Activate"
;; 		    "node-align-vertical"
;; 		    '(:array (:variant "last"))
;; 		    '(:array :signature "{sv}")))

;; Example of opening a file.

(defun jmm-inkscape-open-file (file set-window)
  "Open a file in inkscape.
If SET-WINDOW is non-nil, set current buffer `jmm-inkscape-window-id' as well."
  (interactive
   (list (expand-file-name (buffer-file-name)) t)
   jmm-inkscape-svg-mode)
  (let ((prevwindows (ji--get-inkscape-windows))
	curwindows windiff)
    (dbus-call-method :session "org.inkscape.Inkscape"
		      "/org/inkscape/Inkscape"
		      "org.gtk.Application" "Open"
		      `(:array ,(format "file://%s" file))
		      ""
		      '(:array :signature "{sv}"))
    (setq curwindows (ji--get-inkscape-windows))

    (when set-window
      (if-let* ((windiff (seq-difference curwindows windiff)))
	  (ji-set-window (car windiff))))))

;; (dbus-call-method :session "org.inkscape.Inkscape"
;; 		    "/org/inkscape/Inkscape"
;; 		    "org.gtk.Actions" "Activate"
;; 		    "query-x"
;; 		    '(:array :signature "v")
;; 		    '(:array :signature "{sv}"))

;; This does, in fact, do something, but it ends up printing in the console.
;; (dbus-call-method :session "org.inkscape.Inkscape"
;; 		    "/org/inkscape/Inkscape"
;; 		    "org.gtk.Actions" "Activate"
;; 		    "select-list"
;; 		    '(:array :signature "v")
;; 		    '(:array :signature "{sv}"))

;; (dbus-call-method :session "org.inkscape.Inkscape"
;; 		  (format "/org/inkscape/Inkscape/window/%s" jmm-inkscape-window-id)
;; 		  "org.gtk.Actions" "Activate"
;; 		  "canvas-commands-bar"
;; 		  '(:array :signature "v")
;; 		  '(:array :signature "{sv}"))

(defun jmm-inkscape-launch (file)
  "Launch Inkscape opening FILE.
Runs Inkscape inside of Emacs so we can collect query responses from
  standard output.

Sets `jmm-inkscape-process'.
If `jmm-inkscape-process' is live, call `jmm-inkscape-open-file' instead."
  (interactive
   (list (expand-file-name (buffer-file-name)))
   jmm-inkscape-svg-mode)
  (if (process-live-p jmm-inkscape-process)
      (jmm-inkscape-open-file file t)
      ;; MAYBE: Set some unique D-Bus address
      (setq jmm-inkscape-process (make-process :name "inkscape"
				  :buffer "inkscape"
				  :command (list "inkscape" (expand-file-name (buffer-file-name)))
				  :stderr "*inkscape err*"))
      ;; Calling this too early won't work, and it should just be "1" anyway.
      ;; (ji-set-window (car (ji--get-inkscape-windows)))
      (ji-set-window "1")))

(defun jmm-inkscape-get-selection-ids ()
  "Return a list of IDS of the current selection."
  (let* ((buf (generate-new-buffer " *inkscape selected-list*"))
	 (called nil)
	 (newfilter (lambda (proc string)
		      (with-current-buffer buf
			(setq called t)
			(goto-char (point-max))
			(insert string)))))
    (add-function :before (process-filter jmm-inkscape-process) newfilter)

    (if executing-kbd-macro
	;; There's a weird issue with `dbus-call-method' that prevents it
	;; from working inside of a keyboard macro.  It has something to
	;; do with the fact that it processes events rather than treats a
	;; dbus call like a process.
	;;
	;; This hack works around that, but it's only used inside a macro.
	;;
	;; FIXME: The filter will not be called if the selection is empty,
	;; and thus the function will hang.
	(progn (dbus-call-method-asynchronously
		:session "org.inkscape.Inkscape"
		"/org/inkscape/Inkscape"
		"org.gtk.Actions" "Activate"
		nil
		"select-list"
		'(:array :signature "v")
		'(:array :signature "{sv}"))
	       ;; I'm assuming we only need to be called once.  This might not be accurate.
	       (while (not called)
		 (accept-process-output jmm-inkscape-process)))
       (dbus-call-method
		:session "org.inkscape.Inkscape"
		"/org/inkscape/Inkscape"
		"org.gtk.Actions" "Activate"
		"select-list"
		'(:array :signature "v")
		 '(:array :signature "{sv}")))
    ;; Also, theoretically something could interrupt Inkscape in the
    ;; middle and cause the filter to get more than just the selection.
    (remove-function (process-filter jmm-inkscape-process) newfilter)
    (prog1
	(with-current-buffer buf
	  (goto-char (point-min))
	  (cl-loop while (re-search-forward (rx bol (group (1+ (not blank)))) nil t)
		   collect (match-string-no-properties 1)))
      (kill-buffer buf))))

(defun jmm-inkscape--query-to-string (action)
  "Run a org.gtk.Actions Activate query, returning a string.
ACTION is a string like \"query-height\". "
  (let* ((buf (generate-new-buffer " *inkscape query*"))
	 (called nil)
	 (newfilter (lambda (proc string)
		      (with-current-buffer buf
			(setq called t)
			(goto-char (point-max))
			(insert string)))))
    (add-function :before (process-filter jmm-inkscape-process) newfilter)

    (if executing-kbd-macro
	;; There's a weird issue with `dbus-call-method' that prevents it
	;; from working inside of a keyboard macro.  It has something to
	;; do with the fact that it processes events rather than treats a
	;; dbus call like a process.
	;;
	;; This hack works around that, but it's only used inside a macro.
	;;
	;; FIXME: The filter will not be called if the selection is empty,
	;; and thus the function will hang.
	(progn (dbus-call-method-asynchronously
		:session "org.inkscape.Inkscape"
		"/org/inkscape/Inkscape"
		"org.gtk.Actions" "Activate"
		nil
		action
		'(:array :signature "v")
		'(:array :signature "{sv}"))
	       ;; I'm assuming we only need to be called once.  This might not be accurate.
	       (while (not called)
		 (accept-process-output jmm-inkscape-process)))
       (dbus-call-method
		:session "org.inkscape.Inkscape"
		"/org/inkscape/Inkscape"
		"org.gtk.Actions" "Activate"
		action
		'(:array :signature "v")
		 '(:array :signature "{sv}")))
    ;; Also, theoretically something could interrupt Inkscape in the
    ;; middle and cause the filter to get more than just the selection.
    (remove-function (process-filter jmm-inkscape-process) newfilter)
    (prog1
	(with-current-buffer buf
	  (buffer-substring-no-properties (point-min) (point-max)))
      (kill-buffer buf))))

(defun jmm-inkscape--query-dimension (query clampfun)
  "Get a dimension (x, y, width, height), using CLAMPFUN for multiple values.
For example, if you're querying X, CLAMPFUN should be #'min."
  (thread-first
    (jmm-inkscape--query-to-string query)
    (string-split  ",")
    (thread-last
      (mapcar #'string-to-number)
      (apply clampfun))))

(defun jmm-inkscape--selected-bounds ()
  "Get the outer bounding box of all selected objects.
Returns an alist with 'left, 'top, 'right, 'bottom, 'width, 'height."
  ;; TODO: Round floating point numbers
  (cl-labels ((runquery (querystr)
		(thread-first
		  (jmm-inkscape--query-to-string querystr)
		  (string-split  ",")
		  (thread-last
		    (mapcar #'string-to-number)))))
    (let* ((xs (runquery "query-x"))
	   (ys (runquery "query-y"))
	   (widths (runquery "query-width"))
	   (heights (runquery "query-height"))
	   (left (apply #'min xs))
	   (top (apply #'min ys))
	   (right (apply #'max (cl-mapcar #'+ xs widths)))
	   (bottom (apply #'max (cl-mapcar #'+ ys heights)))
	   (width (- right left))
	   (height (- bottom top)))
      `((left . ,left)
	(top . ,top)
	(right . ,right)
	(bottom . ,bottom)
	(width . ,width)
	(height . ,height)))))

(defun jmm-inkscape--goto-id (id)
  "Go to id.
This is kind of hacky, but should work."
  (if-let* ((loc (save-excursion
		     (goto-char (point-min))
		     (re-search-forward (format "\\bid=[\"']%s[\"']" (regexp-quote id))))))
    (prog1
	(goto-char loc)
      (nxml-backward-up-element))
    (error "Can't find id %s" id)))

(defvar-keymap jmm-inkscape-matches-mode-map
  :doc "Like occur-mode for showing the selection of IDs."
  "n" #'next-error-no-select
  "p" #'previous-error-no-select
  "q" #'quit-window)

(define-derived-mode jmm-inkscape-matches-mode special-mode "InkMatches"
  "Like `occur-mode', but just shows a list of selected Inkscape IDs.

\\{jmm-inkscape-matches-mode-map}"
  (setq-local next-error-function #'jmm-inkscape-id-next-error-function))

(defvar jmm-inkscape-id-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-2] 'jmm-inkscape-id-goto-error)
    (define-key map [follow-link] 'mouse-face)
    (define-key map "\C-m" 'jmm-inkscape-id-goto-error)
    map)
  "Keymap for inkscape ID buttons.")
(fset 'jmm-inkscape-id-button-map jmm-inkscape-id-button-map)

(defun jmm-inkscape-id-goto-error (&optional event)
  "Follow a match."
  (interactive (list last-input-event))
  (when event (posn-set-point (event-end event)))
  (if-let* ((id (get-text-property (point) 'jmm-inkscape-id)))
      (if (buffer-live-p jmm-inkscape-orig-buffer)
	  (progn
	    (pop-to-buffer jmm-inkscape-orig-buffer)
	    (jmm-inkscape--goto-id id))
	(error "Original buffer is gone"))
    (error "No id at point")))

(defun jmm-inkscape-id-next-error-function (arg reset)
  "A `next-error-function' for going to next selected ID."
  (when reset (goto-char (point-min)))
  (let (match)
    (if (< 0 arg)
	(dotimes (_ arg)
	  (setq match (text-property-search-forward 'jmm-inkscape-id nil nil t)))
      (dotimes (_ (abs arg))
	(setq match (text-property-search-backward 'jmm-inkscape-id nil nil t))))
    (if match
	(progn
	  (goto-char (prop-match-beginning match))
	  (jmm-inkscape-id-goto-error))
      (user-error "No more IDs."))))

(defun jmm-inkscape-goto-selected-node ()
  "Go to first selected node."
  (interactive nil nxml-mode)
  (if-let* ((selectedids (jmm-inkscape-get-selection-ids)))
      (let* ((buf (current-buffer))
	     (listbuf (get-buffer-create "*inkscape selection*")))
	(with-current-buffer listbuf
	  (let ((inhibit-read-only t))
	    (erase-buffer)
	    (insert "Selected IDs:\n")
	    (cl-loop for id in selectedids
		     do (progn
			  (insert (propertize id
					      'face 'bold 'jmm-inkscape-id id
					      'mouse-face 'highlight
					      'keymap jmm-inkscape-id-button-map)
				  "\n")))
	    (jmm-inkscape-matches-mode)
	    (goto-char (point-min))
	    (setq-local jmm-inkscape-orig-buffer buf)))
	(setq-local next-error-buffer listbuf) ;; I don't think this actually does anything
	(next-error-select-buffer listbuf)     ;; This does, though.
	(next-error 1 t))
    (user-error "No selected IDs found.")))

(defun jmm-inkscape--read-command-from-key (prompt)
  "Read a command from a keybinding.
PROMPT is a message to display when reading a key.
If M-x is given, read the command."
  (let (cmd)
    (setq cmd (key-binding (read-key-sequence prompt)))
    (if (eq cmd 'execute-extended-command)
	(read-command "Command: ")
      cmd)))

(defun jmm-inkscape-for-each-remaining-selection (fun)
  "Call FUN at point for each remaining selected node."
  (interactive (list (jmm-inkscape--read-command-from-key "For each remaining: "))
	       jmm-inkscape-svg-mode)
  (while (condition-case nil
	     (when-let* ((buffer (next-error-find-buffer)))
	       (with-current-buffer (next-error-find-buffer)
		 (jmm-inkscape-id-next-error-function 1 nil)))
	   (error nil))
    (if (commandp fun  t)
	(call-interactively fun)
      (funcall fun))))

;; TODO: Make a query version like query-replace-regexp.
(defun jmm-inkscape-for-each-selected-node (fun)
  "Call FUN with point at beginning of each selected node."
  (interactive (list (jmm-inkscape--read-command-from-key "For each node: "))
	       jmm-inkscape-svg-mode)
  (cl-loop for id in (jmm-inkscape-get-selection-ids)
	   do (progn
		(jmm-inkscape--goto-id id)
		(if (commandp fun t)
		    (call-interactively fun)
		  (funcall fun)))))

(defun jmm-inkscape-new-macro-for-each-selected-node ()
  "Records and runs a macro on each selected node.
Starts with point at first selected node and starts recording a macro.
Call `exit-recursive-edit' to finish recording the macro and run on all remaining nodes."
  (interactive nil jmm-inkscape-svg-mode)
  ;; Doesn't seem like `atomic-change-group' would work here, probably
  ;; because of the recursive edit.
  (let ((ids (jmm-inkscape-get-selection-ids)))
    (progn
      ;; Go to first node and start recording
      (jmm-inkscape--goto-id (car ids))
      (kmacro-start-macro nil)
      (recursive-edit)
      (kmacro-end-macro nil)
      (cl-loop for id in (cdr ids)
	       do (progn
		    (jmm-inkscape--goto-id id)
		    (call-last-kbd-macro nil))))))

(defun jmm-inkscape-apply-macro-to-selected-nodes (&optional macro)
  "Like `apply-macro-to-region-lines', but applies last macro to selected nodes."
  (interactive nil jmm-inkscape-svg-mode)
  ;; TODO: Interactively prompt for macro, maybe with a keyboard binding.
  (or macro
      (progn
	(if (null last-kbd-macro)
	    (user-error "No keyboard macro has been defined"))
	(setq macro last-kbd-macro)))
  (cl-loop for id in (jmm-inkscape-get-selection-ids)
	   do (progn
		(jmm-inkscape--goto-id id)
		(execute-kbd-macro macro))))

(defun jmm-inkscape--add-class-str (prevval classes)
  "Add space-separated string of CLASSES to string attribute value PREVVAL.
Returns a new string of space-separated classes."
  (mapconcat #'identity
	     (seq-uniq (append (split-string (or prevval ""))
			       (split-string classes)))
	     " "))

;; TODO: Append ";" at end of style.
(defun jmm-inkscape-style-to-class (classname)
  "Try to convert the style to a class."
  (interactive (list (read-string "Class: ")) jmm-inkscape-svg-mode)
  (when-let* ((style (save-excursion
		 (jnx--element-for-point)
		 (jnx--attribute-value "style"))))
    (let* ((stylerx (format "style=\"%s\"" (regexp-quote style)))
	   (matching (save-excursion
		       (goto-char (point-min))
		       (cl-loop while (re-search-forward stylerx nil t)
				collect (save-excursion
					  (jnx--element-for-point)
					  (point))))))
      (message "Found %s matching" matching)
      (when-let* ((huh (save-excursion
			 (goto-char (point-min))
			 (when (jnx--next-tagname "style")
			   (pcase-let* ((`(,start . ,end) (jnx--element-bounds)))
			     (re-search-forward "]]>" nil t)
			     (match-beginning 0))))))
	(atomic-change-group
	  (goto-char huh)
	  (insert (format ".%s { %s }\n" classname style))
	  (goto-char (point-min))
	  (cl-loop while (re-search-forward stylerx nil t)
		   do (save-excursion
			(jnx--element-for-point)
			(jnx--set-attribute-value "style" nil)
			(jnx--update-attribute-value "class"
						     #'jmm-inkscape--add-class-str
						     nil
						     classname))))))))

;; TODO: Iterate through styles, count how often each style is repeated.

;; TODO: Check if IDs are referenced elsewhere
;; TODO: Add option to strip only Inkscape generated IDs (which have
;; the tag plus a series of numbers)
(defun jmm-inkscape-strip-ids (beg end)
  "Try to convert the style to a class."
  (interactive (if (use-region-p)
		   (list (region-beginning) (region-end))
		 (let ((bounds (save-excursion
				 (jnx--element-for-point)
				 (jnx--element-bounds))))
		   (list (car bounds) (cdr bounds))))
	       jmm-inkscape-svg-mode)
  (let ((markend (point-marker))
	(count 0))
    (set-marker markend end)
    (atomic-change-group
      (save-excursion
	(goto-char beg)
	(while (jnx--next-tagname
		(lambda () (jnx--attribute-value "id")) markend)
	  (jnx--set-attribute-value "id" nil)
	  (cl-incf count))))
    (message "Removed %d ids" count)
    (set-marker markend nil)))

(defun jmm-inkscape-strip-translation ()
  "Remove transform-translate for node, incorporating into x and y coordinates.

Removes transform=\"translate(dx,dy)\" and adds it to x= and y=.

Useful since I can't find out how to move text in Inkscape
without causing a transform, which prevents bounding box live
path effects from working.

Might be related to https://gitlab.com/inkscape/inkscape/-/issues/3663"
  (interactive nil jmm-inkscape-svg-mode)
  (atomic-change-group
    (save-excursion
      (jnx--element-for-point)
      (if-let* ((x (jnx--attribute-value "x"))
		(y (jnx--attribute-value "y"))
		(transform (jnx--attribute-value "transform")))
	  (pcase-let* (((rx "translate(" (let dx (1+ nonl)) "," (let dy (1+ nonl)) ")") transform))
	    (unless (and dx dy) (error "Can't find or parse translate() transform attribute."))
	    (let ((x1 (string-to-number x))
		  (y1 (string-to-number y))
		  (dx1 (string-to-number dx))
		  (dy1 (string-to-number dy)))
	      (jnx--set-attribute-value "x" (number-to-string (+ x1 dx1)))
	      (jnx--set-attribute-value "y" (number-to-string (+ y1 dy1)))
	      (jnx--set-attribute-value "transform" nil)))
	;; Should it not error?
	(error "Missing either x, y, or transform attribute.")))))

;;; xref backend for SVGs

;; This xref backend lets me easily jump to definitions for things
;; like patterns and clones (i.e., <use>).  It also lets me see where
;; elements are referenced elsewhere (like with <use> elements).

;; TODO: Get a list of all classes as well.
;; 	 There's probably an easy way to hook into the new treesit library
;; 	 so I don't have to parse things myself.

;; TODO: Some elements may not have ids (though this isn't the case
;; 	 for Inkscape SVGs).  We should return some kind of marker for
;; 	 them.

;; TODO: What's `xref-backend-apropos' for, and should I implement it?

;; TODO: This isn't Inkscape-specific, so maybe it should be renamed
;; 	 to jmm-svg-location, for example.

;; I might be cargo-culting the xref elisp backend too much.  Having
;; "symbol", "type", and "file" (from `xref-elisp-location') may be
;; more flexible and might not need to be named this way.  See
;; `xref-file-location' for example.

(defun ji--get-all-ids (&optional buffer)
  "Get all ids in an SVG buffer.
Returns a list of strings."
  (let (ids)
    (with-current-buffer (or buffer (current-buffer))
      (let* ((dom (libxml-parse-xml-region (point-min) (point-max)))
	     (ids (cl-loop for elem in (dom-by-id dom ".")
			   append (when-let* ((id (dom-attr elem 'id)))
				    ;; MAYBE: Propertize with some "id" type, compared to a class?
				    (list id)))))
	(delete-dups ids)))))

(defun ji--id-at-point ()
  "Try to return a string ID around point.
Returns nil if an ID isn't found, which causes xref to prompt for an ID."
  ;; MAYBE: If we're not directly on an id, we could get the id of the
  ;; current element or look for an xlink:href.
  (save-excursion
    (let* ((start (progn (skip-chars-backward "[:alnum:]-_") (point)))
	   (end (progn (skip-chars-forward "[:alnum:]-_") (point)))
	   (maybeid (buffer-substring-no-properties start end))
	   (allids (ji--get-all-ids))) ;; TODO: Cache this, probably using buffer modified tick.
      (when (member maybeid allids)
	maybeid))))

(defun ji--xref-goto-id (id)
  "Find where an ID is defined.
Only goes to the attribute, not the beginning of the element."
  (when-let* ((loc (save-excursion
		     (goto-char (point-min))
		     (re-search-forward (format "id=[\"']%s[\"']" (regexp-quote id))))))
    (goto-char loc)
    ;; (nxml-backward-up-element)
    ))

(defun ji--make-xref-for-element ()
  "Assuming we just scanned an element and are at its point, return an xref."
  (save-excursion
    (jnx--element-for-point) ;; Rescan out of paranoia?
    (ji--xref-make-xref (xmltok-start-tag-qname) (jnx--attribute-value "id") (buffer-file-name))))

(defun ji--find-references (id)
  "Find all mentions of #id in current buffer.
Returns a list of xrefs (specifically `xref-match-item' so we can perform replacements)."
  ;; FIXME: Should an item reference itself?
  ;; It's useful for renaming ids (though it's easy to do this with "M-s .")
  (let ((file (buffer-file-name))
	refs)
    (save-excursion
      (save-restriction
	(widen)
	(goto-char (point-min))
	;; Use "#\\(%s\\)\\>" if you don't want self references.
	(cl-loop while (re-search-forward (format "\\<\\(%s\\)\\>" (regexp-quote id)) nil t)
		 collect (ji--xref-make-match 1))))))

(cl-defstruct (xref-jmm-inkscape-location
               (:constructor xref-make-jmm-inkscape-location (symbol type file)))
  "Location of an ID definition.
SYMBOL is the id.
TYPE is the qname of the tag.
FILE is the file it's located in.

This is mostly cargo-culted from `xref-elisp-location'."
  symbol type file)

(cl-defstruct (xref-jmm-inkscape-match-location
               (:constructor xref-make-jmm-inkscape-match-location (symbol type file)))
  "Location of a match, probably for references to an ID.
SYMBOL is the id.
TYPE is the qname of the tag.
FILE is the file it's located in.

This is mostly cargo-culted from `xref-elisp-location'."
  symbol type file)

(defvar ji--xref-format
  ;; This is used to format stuff for the *xref* buffer, for example
  ;; when finding references.
  ;; Maybe: We might want to return the Inkscape label of something.
  #("<%s %s>"
    1 3 (face nxml-element-local-name)
    4 6 (face font-lock-variable-name-face)))

(defun ji--xref-make-xref (type symbol file &optional summary)
  "Return an xref for TYPE SYMBOL in FILE.
If SUMMARY is non-nil, use it for the summary; otherwise the
summary is formatted using `jmm-inkscape--xref-format' with the
TYPE and SYMBOL.

For now, TYPE is a string returning the element name
and SYMBOL is a string representing an ID.
FILE is included, but currently we only look at the current file."
  ;; Is it appropriate for type to be a qname? Or should these all be some "element" type?
  ;; Type could be like 'element vs 'class.
  (xref-make (or summary
		 (format ji--xref-format type symbol))
	     (xref-make-jmm-inkscape-location symbol type file)))

(defun ji--xref-make-match (subexp)
  "See `xref-match-item'.
Assume we just matched some reference.
Pass in SUBEXP of the match data to use.
Returns an xref that's also a match item, so it can be used for `xref-query-replace-in-results'."
  ;; Calculate these here because jnx--element-for-point might mess with match data.
  (let* ((loc (xref-make-buffer-location (current-buffer) (match-beginning subexp)))
	 (len (- (match-end subexp) (match-beginning subexp)))
	 (matchstr (match-string-no-properties subexp)))
    (xref-make-match
     ;; Hack for `xref-query-replace-in-results' because of an
     ;; assumption in `xref--outdated-p'.
     (propertize
      matchstr
      ;; This is a hack to display *where* a match occurs because
      ;; `xref--outdated-p' assumes the summary is the same as the
      ;; region between location and match length.
      'display
      (save-excursion
	(jnx--element-for-point)
	(format ji--xref-format (xmltok-start-tag-qname) (jnx--attribute-value "id"))))
     loc
     len)))

(defun ji--xref-find-definitions (symbol)
  "Returns a list of xrefs for definitions of SYMBOL."
  (when (ji--xref-goto-id symbol)
    ;; In the future, we could return multiple definition locations
    ;; for things like classes.
    (list (ji--make-xref-for-element))))

(defun ji--xref-backend ()
  ;; TODO: Is there a time we should return a different backend or nil?
  ;; Like, is there a time when this backend isn't applicable?
  'jmm-inkscape)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql 'jmm-inkscape)))
  ;; Note: I used to propertize the returned string with a 'pos, but I
  ;; didn't use it.  In the future, we might propertize the string
  ;; with a type.  For example, to show that an identifier is a class
  ;; name instead of an element ID.
  (ji--id-at-point))

(cl-defmethod xref-backend-identifier-completion-table ((_backend
                                                         (eql 'jmm-inkscape)))
  ;; This is used when prompting for an ID.
  ;; It could theoretically be annotated, grouped, etc.
  ;; It also might use Inkscape labels or <title> elements.
  (let ((buf (current-buffer)))
    (completion-table-dynamic
     (lambda (_)
       (ji--get-all-ids buf)))))

(cl-defmethod xref-backend-definitions ((_backend (eql 'jmm-inkscape)) identifier)
  ;; MAYBE: Check properties on the "identifier" string to see what
  ;; kind of identifier it is.  For example, it could be a class,
  ;; element, or something else.
  (ji--xref-find-definitions identifier))

(cl-defmethod xref-backend-references ((_backend (eql 'jmm-inkscape)) identifier)
  ;; MAYBE: Scan ids in all project files using ripgrep?
  (ji--find-references identifier))

(cl-defmethod xref-location-group ((l xref-jmm-inkscape-location))
  ;; Needed for `xref-backend-references'
  ;; I think we could also group things by, for example, layer.
  ;; This basically just returns a string, I think.
  (xref-jmm-inkscape-location-file l))

(cl-defmethod xref-location-marker ((l xref-jmm-inkscape-location))
  ;; Should this return the point of the reference or the point of the element?
  ;; For `xref-query-replace-in-results' it seems like it should start exactly at the ID.
  (pcase-let (((cl-struct xref-jmm-inkscape-location symbol type file) l))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
	(save-restriction
          (widen)
	  (goto-char (point-min))
	  ;; This is a hack, but it's close enough for now.
	  (re-search-forward (format "id=[\"']\\(%s\\)[\"']" (regexp-quote symbol)))
	  (goto-char (match-beginning 1))
	  ;; (nxml-backward-up-element)
          (point-marker))))))

;;; continue other stuff.

;;;###autoload
(defun ji-svg-insert-image (file)
  "Embed an image tag, adding the appropriate width and height.
Uses a relative file name."
  ;; Note: This isn't really inkscape-specific.
  ;; Maybe it should just be called "jmm-svg-insert-image".
  (interactive (list (read-file-name "Image file: ")) jmm-inkscape-svg-mode)
  (save-excursion
    (insert "<image preserveAspectRatio=\"none\" style=\"image-rendering:optimizeQuality\" x=\"0\" y=\"0\" />"))
  (jnx-at-element-start
    (jnx--set-attribute-value "xlink:href" (file-relative-name (expand-file-name file)))
    (let ((size (image-size (create-image file) t)))
      (jnx--set-attribute-value "width"  (number-to-string (car size)))
      (jnx--set-attribute-value "height" (number-to-string (cdr size))))))

;;;###autoload
(defun ji-svg-element-bounds-from-viewbox ()
  "Set the element's dimensions to be same size as viewbox."
  (interactive nil jmm-inkscape-svg-mode)
  (jnx-at-element-start
    (let* ((dom (libxml-parse-xml-region (point-min) (point-max)))
	   (svgnode (car (dom-by-tag dom 'svg)))
	   (viewbox (dom-attr svgnode 'viewBox)))
      (pcase-let* (((rx (let x (1+ graph)) " "
			(let y (1+ graph)) " "
			(let width (1+ graph)) " "
			(let height (1+ graph)))
		    viewbox))
	(unless (and x y) (error "Can't find or parse viewbox attribute."))
	(jnx--set-attribute-value "x" x)
	(jnx--set-attribute-value "y" y)
	(jnx--set-attribute-value "width" width)
	(jnx--set-attribute-value "height" height)))))

;;;###autoload
(defun ji-svg-element-set-dimensions-from-width (newwidth)
  "Set the element's dimensions from a new width."
  ;; TODO: Handle old width with units.  For example, "500px".
  (interactive (list (string-to-number (read-string "New width: "))) jmm-inkscape-svg-mode)
  (jnx-at-element-start
    ;; Numbers may be integers, convert to float.
    (let* ((width  (* 1.0 (string-to-number (jnx--attribute-value "width"))))
	   (height (* 1.0 (string-to-number (jnx--attribute-value "height"))))
	   (ratio (/ newwidth width))
	   (newheight (* ratio height)))
      (atomic-change-group
	(jnx--set-attribute-value "width" (number-to-string newwidth))
	(jnx--set-attribute-value "height" (number-to-string newheight))))))

;;;###autoload
(defun ji-svg-element-bounds-from-selection ()
  "Set the element's dimensions from outer bounding box of selection.
See `jmm-inkscape--selected-bounds'."
  (interactive nil jmm-inkscape-svg-mode)
  (jnx-at-element-start
    (pcase-let* (((map left top width height) (jmm-inkscape--selected-bounds)))
      (jnx--set-attribute-value "x" (number-to-string left))
      (jnx--set-attribute-value "y" (number-to-string top))
      (jnx--set-attribute-value "width" (number-to-string width))
      (jnx--set-attribute-value "height" (number-to-string height))
      )))

(defun ji--read-delta ()
  "Get delta value from interactive prefix."
  (let ((defaultdelta 10))
    (pcase current-prefix-arg
      ('- (- defaultdelta))
      ((pred null) defaultdelta)
      (t (prefix-numeric-value current-prefix-arg)))))

;;;###autoload
(defun ji-svg-element-bounds-adjust-delta (delta)
  "Adjust the element's dimensions by DELTA units in all directions."
  (interactive (list (ji--read-delta))
	       jmm-inkscape-svg-mode)
  (let ((add1 (lambda (oldx)
		(number-to-string (- (string-to-number oldx) delta))))
	(add2 (lambda (oldwidth)
		(number-to-string (+ (string-to-number oldwidth) (* delta 2))))))
    (jnx-at-element-start
      (jnx--update-attribute-value "x" add1)
      (jnx--update-attribute-value "y" add1)
      (jnx--update-attribute-value "width" add2)
      (jnx--update-attribute-value "height" add2))))

;;;###autoload
(defun ji-svg-insert-viewbox-rectangle ()
  "Insert a rectangle the same size as the viewport."
  (interactive nil jmm-inkscape-svg-mode)
  (save-excursion
    (insert "<rect style=\"opacity:0.3;fill:#deddda\" />"))
  (ji-svg-element-bounds-from-viewbox))

;;;###autoload
(defun ji-svg-insert-bounding-rectangle (&optional delta)
  "Insert a rectangle the same size as the selection."
  (interactive (list
		(when current-prefix-arg
		  (prefix-numeric-value current-prefix-arg)))
	       jmm-inkscape-svg-mode)
  (save-excursion
    (insert "<rect style=\"opacity:0.3;fill:#deddda\" />"))
  (ji-svg-element-bounds-from-selection)
  (when delta
    (ji-svg-element-bounds-adjust-delta delta)))

;; Note: I still haven't found a way to determine which windows belong
;; to which documents/files.
(defun ji--get-inkscape-windows ()
  "Get a list of window names, sorted in descending numeric order."
  (seq-sort-by #'string-to-number
	       #'>
	       (dbus-introspect-get-node-names
		:session "org.inkscape.Inkscape" "/org/inkscape/Inkscape/window")))

(defun ji-set-window (window)
  "Set the Inkscape window for the current buffer."
  (interactive
   (list (if-let* ((windows (ji--get-inkscape-windows))
		   (default (car windows)))
	     (if (length= windows 1)
		 default
	       (completing-read (format-prompt "Inkscape window" default) windows nil t nil nil default))
	   (error "No Inkscape windows found")))
   jmm-inkscape-svg-mode)
  (setq-local jmm-inkscape-window-id window)
  (message "Inkscape window ID set to %s" jmm-inkscape-window-id window))

(defun ji--maybe-reload ()
  "Possibly reload the Inkscape window."
  (when (and jmm-inkscape-auto-reload-mode
	     jmm-inkscape-window-id)
    (jmm-inkscape-revert)))

;; TODO: This gets called repeatedly for some reason. Like 4 times in
;; a row. Maybe we should cache the Inkscape windows.
;; Also, sometimes the menu is weirdly set to "About GNU"
(defun ji--set-window-id-menu (_menu-def)
  "Tries to make a menu for selecting the current window ID."
  (when-let* ((windows (ji--get-inkscape-windows))
	      (default (car windows)))
    (mapcar (lambda (win)
	      `[,(if win
		     (format "Window %s " win)
		   "None")
		,(lambda ()
		   (interactive)
		   (ji-set-window win))
		:style radio
		:selected (eval (equal jmm-inkscape-window-id
				       ,win))])
	    (cons nil windows))))

(defvar-keymap jmm-inkscape-svg-mode-map
  :parent nxml-mode-map
  "C-M-a" #'jnx-beginning-of-inner-sexp
  "C-M-e" #'jnx-end-of-inner-sexp
  "M-<up>" #'jnx-transpose-element-up
  "M-<down>" #'jnx-transpose-element-down
  "C-c C-e C-a" #'jnx-edit-attribute
  "C-c C-e C-s" #'jnx-swap
  "C-c C-e C-w" #'jnx-wrap
  "C-c C-w" #'jnx-wrap
  "C-c C-e C-u" #'jnx-unwrap
  "C-c C-e C-b" #'jnx-blockify-element
  "C-c C-e C-i" #'jnx-inline-element
  "C-c C-e C-f" #'jnx-flatten-element
  "C-c C-e r B" #'ji-svg-insert-bounding-rectangle
  "C-c C-e r V" #'ji-svg-insert-viewbox-rectangle
  "C-c C-e r b" #'ji-svg-element-bounds-from-selection
  "C-c C-e r +" #'ji-svg-element-bounds-adjust-delta
  "C-c C-e r v" #'ji-svg-element-bounds-from-viewbox
  ;; TODO: Add jmm-xhtml-set-class
  ;; "C-c C-e C-c" #'jmm-xhtml-set-class
  "C-M-<backspace>" #'jmm/nxml-unwrap
  "M-]" #'jnx-out-of-element
  "C-c C-r" #'jmm-inkscape-revert
  "C-c C-j C-l" #'jmm-inkscape-launch
  "C-c C-j C-w" #'jmm-inkscape-set-window
  "C-c C-s C-s" #'jmm-inkscape-selection-add-node
  "C-c C-s C-l" #'jmm-inkscape-selection-refresh
  "C-c C-s j" #'jmm-inkscape-selection-jump-to-register
  "C-c C-s i" #'jmm-inkscape-selection-insert-register
  "C-c C-k C-n" #'jmm-inkscape-new-macro-for-each-selected-node
  "C-c C-k C-s" #'jmm-inkscape-apply-macro-to-selected-nodes
  "M-g s" #'jmm-inkscape-goto-selected-node
  "M-g M-s" #'jmm-inkscape-goto-selected-node
  "M-n" #'jnx-goto-next-tag
  "M-p" #'jnx-goto-prev-tag)

(easy-menu-define jmm-inkscape-svg-menu jmm-inkscape-svg-mode-map
  "JMM Inkscape SVG Menu."
  `("jmm-SVG"
    ["Launch Inkscape" jmm-inkscape-launch
     :help "Launch an Inkscape subprocess for the current file"]
    ["Set Inkscape Window" jmm-inkscape-set-window
     :help "Set the Inkscape window ID for current buffer"]
    ["Go to Inkscape selection" jmm-inkscape-goto-selected-node
     :help "Go to the object(s) currently selected in Inkscape"]
    ("Set window to"
     :filter ji--set-window-id-menu)
    "--"
    ["Revert Inkscape" jmm-inkscape-revert
     :help "Tell Inkscape to revert the current document"]
    ["Revert Buffer" revert-buffer
     :help "Revert current buffer"]
    ["Auto Revert Inkscape" jmm-inkscape-auto-reload-mode
     :style toggle
     :selected (eval jmm-inkscape-auto-reload-mode)
     :help "Automatically revert Inkscape after a buffer save"]
    ["Auto Revert Buffer" auto-revert-mode
     :style toggle
     :selected (eval auto-revert-mode)
     :help "Automatically revert current buffer"]
    "--"
    ("Insert"
     ["Image" jmm-inkscape-svg-insert-image
      :help "Embed a relative image"]
     ("Rectangle"
      ["Viewport sized" ji-svg-insert-viewbox-rectangle
       :help "Insert rectangle the size of the viewbox"]
      ["Bounding box" ji-svg-insert-bounding-rectangle
       :help "Insert rectangle that bounds the selection"]))))

;; TODO: Remember `jmm-inkscape-window-id', which gets reset if we
;; switch using `image-minor-mode'.

;;;###autoload
(define-derived-mode jmm-inkscape-svg-mode nxml-mode "jInkSVG"
  "Edit SVGs with `nxml-mode' with conveniences for interacting with Inkscape"
  :after-hook
  ;; Set up some alignment
  (setf (alist-get 'jmm-inkscape-attributes align-mode-rules-list)
	'((regexp   . "\\(\\s-*\\) [-a-zA-Z0-9:]+=\"")
	  (repeat   . t)
	  (spacing  . 0)
	  (modes    . '(jmm-inkscape-svg-mode))))
  (rng-set-vacuous-schema)
  (rng-validate-mode -1)
  (add-hook 'xref-backend-functions #'ji--xref-backend nil t))

(add-hook 'jmm-inkscape-svg-mode-hook 'jmm-inkscape-auto-reload-mode)

;;;###autoload
(define-minor-mode jmm-inkscape-auto-reload-mode
  "Syncs changes between buffer and Inkscape.
When the file is saved, Inkscape is notified to revert the file.
When Inkscape changes the file, `auto-revert-mode' automatically reverts the buffer."
  :lighter " inkRev"
  (if jmm-inkscape-auto-reload-mode
      (progn
	(auto-revert-mode 1)
	(add-hook 'after-save-hook #'ji--maybe-reload nil t))
    (remove-hook 'after-save-hook #'ji--maybe-reload t)))

(provide 'jmm-inkscape)
;;; jmm-inkscape.el ends here
;; Local Variables:
;; read-symbol-shorthands: (("ji-" . "jmm-inkscape-") ("jnx-" . "jmm/nxml-"))
;; End:
