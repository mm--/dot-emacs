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

;;; Code:
(require 'align)
(require 'dbus)
(require 'jmm-nxml)

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
  (interactive nil jmm-inkscape-mode)
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
		 (let ((bounds(save-excursion
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
  (interactive nil jmm-inkscape-mode)
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
    ))

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
  (rng-validate-mode -1))

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
