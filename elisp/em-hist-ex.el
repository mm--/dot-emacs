;;; eshell-expanded-history.el --- Add extra metadata to eshell history

;; Copyright (C) 2019 Josh Moller-Mara

;; Author: Josh Moller-Mara <jmm@cns.nyu.edu>
;; Version 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary

;; eshell-expanded-history.el is a package that intends to be an
;; eshell module that adds extended features to eshell history. For
;; example, it remembers which commands were executed in particular
;; directories, how long they took, and whether or not they exited
;; successfully.

;; TODO: Lint this file

;; Expanded history stuff for eshell
;; Besides storing the command line, store things like:
;; - Directory the command was run in
;; - Time the command was run?
;; - Time it took to run the command
;; - Whether the command returned an error
;; - Maybe the number of lines returned?
;; - Whether it was remote (TRAMP)

;; That kind of thing.
;; That way I can use command history and not re-use a command where I
;; had a typo and it errored out.

;; Getting the directory
;; Either use the variable default-directory
;; Or the command (eshell/pwd)

;; Hooks for command input
;; eshell-expand-input-functions?

;; eshell-pre-command-hook
;; -> This doesn't take an argument. How to get the command?
;; I think probably using 'eshell-expand-input-functions might be best here.

;; Should I have one giant eshell history ring?
;; Filter things by buffer?

;; DONE: Be able to save my extended history to a file.
;; (Take the cons of everything in the ring, and write it to a file.)
;; DONE: Be able to read my extended history from a file.
;; (Opposite of above. Buffer I suppose should be nil.)

;; TODO: Ignore duplicates in some same manner (same buffer,
;; directory, and command)?

;; TODO: Add ability to CD directly and re-run if in a different
;; directory. (Partially done by mouse clicks. Should be done with
;; ring numbers though.)

;; TODO: Refer to dhist ring by number?

;; TODO: Add arguments to dhist to filter by success (DONE), failure (DONE), elapsed
;; time (DONE), how old it is, etc.
;; - DONE: Parse human readable time? (like 2m, 30sec, etc.)
;;         - This can be done with "timer"'s "timer-duration" function
;;         - It'd be nice to go the other way around

;; TODO: Import zshell history or bash?

;; DONE: Make a hotkey to notify when a command is done.
;;  - TODO: Find out how to see if a buffer is visible or focused
;;  - TODO: Add optional modeline status of running jobs being tracked.


;; DONE: Use only the host name and directory for dhist (It shouldn't
;; matter if we're using rsync, ssh, sshx, etc since the directory
;; should be the same)

;;; Code:

(require 'ring)
(require 'eshell)

;;;###autoload
(progn
  (defgroup eshell-hist-ex nil
  "This module provides extended command history management."
  :tag "Extended history list management"
  :group 'eshell-module))

;;; User Variables:

(defcustom eshell-hist-ex-file-name
  (expand-file-name "hist-ex.gz" eshell-directory-name)
  ;; TODO: Rewrite
  "Name of the file to read/write input history.
See also `eshell-read-history' and `eshell-write-history'."
  :type 'file
  :group 'eshell-hist-ex
  :risky t)

;; TODO: Have separate history size for eshell-hist-ex?

;;; Interval Variables:

(defvar-local jmm/eshell-last-command-info '()
  "Save information of the last command we ran. This is a cons of
  the eshell buffer and a plist containing information such as :input, :dir, :elapsed, :exit (code) etc.")

(defvar-local jmm/eshell-tracking-last-command nil
  "Are we tracking information about the last command? This is
  nil if we don't pass `eshell-input-filter' (for example, the command starts with a space.)")

(defvar-local jmm/eshell-alerting-last-command nil
  "Are we alerting when the last command has finished?")

;; Not a local variable because many shells can alert.
(defvar jmm/eshell-last-alert-info nil
  "Information about the last alert.
Useful if we want to go to the buffer the last alert came from.")

(defvar jmm/eshell-expanded-history-ring nil
  "A history ring that is shared among all eshell buffers.
Unlike em-hist.el, I want this to be global across all buffers.")

;; (time-to-seconds (time-subtract (current-time) blah))

;;; Functions

;; TODO: Probably should save buffer if we're gonna have a global ring.
;; Save also if something's finished or not.
(defun jmm/eshell-ext-hist-pre-command-fn ()
  "Gather some data about the eshell input we're about to run.
Take a look at `eshell-add-to-history'.
Hook this into `eshell-input-filter-functions'."
  (when (> (1- eshell-last-input-end) eshell-last-input-start)
    (let ((input (buffer-substring-no-properties eshell-last-input-start
						 (1- eshell-last-input-end)))
	  (dir (eshell/pwd))
	  (beginning-timestamp (current-time))
	  ;; Human readable time
	  (thetime (current-time-string)))
      (if (funcall eshell-input-filter input)
	  (progn
	    (setq jmm/eshell-tracking-last-command t)
	    (setq jmm/eshell-last-command-info
		  (list
		   :input input
		   :dir dir
		   :time thetime
		   :beginning beginning-timestamp))
	    ;; (message "The command was: \"%s\" executed in \"%s\" at \"%s\"" input dir thetime)
	    )
	(setq jmm/eshell-tracking-last-command nil
	      jmm/eshell-last-command-info nil)))))

(add-hook 'eshell-input-filter-functions #'jmm/eshell-ext-hist-pre-command-fn)

(defun jmm/eshell-hist-ext--add-to-history (info)
  "Add some info to the `jmm/eshell-expanded-history-ring'.
If that's nil, initialize it by reading in history."
  (unless jmm/eshell-expanded-history-ring
    (jmm/eshell-hist-ex-read-history))
  (ring-insert jmm/eshell-expanded-history-ring info))

(defun jmm/eshell-ext-hist-post-command-fn ()
  "Gather data about the last command.
Hook this into `eshell-post-command-hook'."
  ;; (message "Post command called")
  (when jmm/eshell-tracking-last-command
    (let* ((input (buffer-substring-no-properties eshell-last-input-start (1- eshell-last-input-end)))
	   (num-lines (count-lines (eshell-beginning-of-output) (eshell-end-of-output)))
	   (exit-code eshell-last-command-status)
	   (elapsed (time-to-seconds (time-subtract (current-time)
						    (plist-get jmm/eshell-last-command-info :beginning)))))
      (cl-remf jmm/eshell-last-command-info :beginning)
      (setq jmm/eshell-last-command-info
	    (cons (current-buffer)
		  (append (list
			   :lines num-lines
			   :elapsed elapsed
			   :exit exit-code)
			  jmm/eshell-last-command-info)))
      ;; Sometimes this gets triggered by non-input commands?
      (setq jmm/eshell-tracking-last-command nil)
      (jmm/eshell-hist-ext--add-to-history jmm/eshell-last-command-info)
      (when jmm/eshell-alerting-last-command
	(jmm/eshell-command-alert (current-buffer) input exit-code elapsed num-lines)
	;; Only alert once unless we're set to alert "always"
	(unless (eq jmm/eshell-alerting-last-command 'always)
	  (setq jmm/eshell-alerting-last-command nil)))
      
      ;; (message "Post-command (input \"%s\") outputted %d lines in %.2f seconds and exited with code \"%d\"" input num-lines elapsed exit-code)
      )))

(add-hook 'eshell-post-command-hook #'jmm/eshell-ext-hist-post-command-fn)

(defun jmm/eshell-toggle-alerting (&optional arg)
  "Toggle `jmm/eshell-alerting-last-command'.
With ARG, toggle between 'always and nil."
  (interactive "P")
  (setq jmm/eshell-alerting-last-command
	(if arg
	    (if jmm/eshell-alerting-last-command nil 'always)
	  (if jmm/eshell-alerting-last-command nil t)))
  (message "Alerting set to %s" jmm/eshell-alerting-last-command))

(bind-key "M-A" #'jmm/eshell-toggle-alerting eshell-mode-map)

(defun jmm/eshell-command-alert (buffer cmdinput exit-code elapsed num-lines)
  "Send `alert' with severity based on STATUS when PROCESS finished.
BUFFER is the eshell buffer being run in.  CMDINPUT is the command that was executed.
EXIT-CODE is how that command exited, as a number.  ELAPSED is the number of seconds taken.
NUM-LINES is the number of lines the command returned.
Modified from https://blog.hoetzel.info/post/eshell-notifications/ which I found through https://irreal.org/blog/?p=7469"
  (let* ((msg (format "Command \"%s\": %s in %.2f seconds and outputted %d lines."
		      cmdinput
		      (if (= exit-code 0)
			  "Exited successfully"
			(format "Exited abnormally with code \"%d\"" exit-code))
		      elapsed
		      num-lines)))
    (setq jmm/eshell-last-alert-info
	  (list :buffer    buffer
		:cmdinput  cmdinput
		:exit-code exit-code
		:elapsed   elapsed
		:num-lines num-lines))
    (if (= exit-code 0)
	(alert msg :buffer buffer :severity 'normal)
      (alert msg :buffer buffer :severity 'urgent))))

(alert-add-rule :status  '(buried visible)     ;only send alert when buffer not visible
		:mode  'eshell-mode
		:style 'notifications)

(defun jmm/eshell-go-to-last-alert (&optional arg)
  "Switch to the buffer where the last alert happened.
With ARG, switch in another window."
  (interactive "P")
  (-if-let (buffer (plist-get jmm/eshell-last-alert-info :buffer))
      (if arg
	  (switch-to-buffer-other-window buffer)
	(switch-to-buffer buffer))
    (user-error "Could not find the buffer of the last alert (if it even existed)")))

;; TODO: Read history when eshell is loaded, or when this file is
;; loaded and active eshell sessions exist
(defun jmm/eshell-hist-ex-read-history (&optional filename)
  "Read in values for `jmm/eshell-expanded-history-ring' from
  FILENAME or `eshell-hist-ex-file-name' by default."
  (let ((file (or filename eshell-hist-ex-file-name)))
    (cond
     ((or (null file)
	  (equal file ""))
      nil)
     ((not (file-readable-p file))
      (message "Cannot read extended history file %s" file))
     (t
      (let* ((count 0)
	     (size eshell-history-size)
	     (ring (make-ring size))
	     ;; TODO: Make eshell-hist-ex-file-name risky
	     (ringlist (with-temp-buffer
			 (insert-file-contents file)
			 (goto-char (point-min))
			 (read (current-buffer)))))
	(--each ringlist (ring-insert ring it))
	(setq jmm/eshell-expanded-history-ring ring))))))

;; Note. We probably don't want to read history before writing our
;; known history first, otherwise we may erase our history.

;; TODO: We probably want to write history every once in a while. (Like if we start another emacs.)

(defun jmm/eshell-hist-ex-write-history (&optional filename append)
  "Write values for `jmm/eshell-expanded-history-ring' to FILENAME or `eshell-hist-ex-file-name' by default.
With APPEND, add to the end of the ring."
  (let ((file (or filename eshell-hist-ex-file-name)))
    (cond
     ((or (null file)
	  (equal file ""))
      nil)
     ((not (file-writable-p file))
      (message "Cannot write extended history file %s" file))
     (t
      (with-temp-buffer
	(goto-char (point-min))
	(pp (->> (ring-elements jmm/eshell-expanded-history-ring)
		  reverse
		  (-map #'cdr)
		  ;; Erase buffer
		  (--map (cons nil it)))
	    (current-buffer))
	(eshell-with-private-file-modes
	 (write-region (point-min) (point-max) file append
		       'no-message)))))))

(add-hook 'eshell-exit-hook #'jmm/eshell-hist-ex-write-history)

;;;;;;;;;;

;; TODO: Handle localhost and sudo? Or should sudo remain separate?
(defun jmm/equal-file-path (file-path1 file-path2)
  "Check if FILE-PATH1 and FILE-PATH2 refer to the same file path,regardless of connection method.
 I usually use this to compare directories.
File names need to be expanded.  Can't tell if two hosts with different names are the same.
For example, doesn't handle localhost yet."
  (and (eq (tramp-tramp-file-p file-path1) (tramp-tramp-file-p file-path2))
       (if (tramp-tramp-file-p file-path1)
	   (let* ((vec1 (tramp-dissect-file-name file-path1))
		  (vec2 (tramp-dissect-file-name file-path2)))
	     (and (string=
		   (tramp-file-name-host vec1)
		   (tramp-file-name-host vec2))
		  (string= 
		   (tramp-file-name-localname vec1)
		   (tramp-file-name-localname vec2))))
	 (string= file-path1 file-path2))))

(defun jmm/eshell-directory-history ()
  "Show commands that were run in this directory."
  (let* ((curdir (eshell/pwd)))
    (unless jmm/eshell-expanded-history-ring
      (jmm/eshell-hist-ex-read-history))
    (->> (ring-elements jmm/eshell-expanded-history-ring)
	 (-map #'cdr)
	 (--filter (jmm/equal-file-path (plist-get it :dir) curdir)))))

(defun jmm/eshell--format-expanded-history (info &optional nodir)
  "Format a string about an input and things like if it failed and how long it took.
INFO is a plist containing various information/data about a command.
Optional NODIR says to not include the directory in the string (such as for `eshell/dhist')."
  (-let (((&plist :input :lines :elapsed :exit :dir) info))
    (propertize
     (->> (list
	   (concat "\""
		   (propertize input
			       'face (if (= exit 0) 'success 'error)
			       'help-echo "SPC, mouse-1: Insert this command at input"
			       'mouse-face 'highlight
			       'keymap jmm/eshell-hist-ex-dhist-keymap)
		   "\"")
	   (unless (= exit 0)
	     (propertize (format "(exit %d)" exit)
			 'face 'error))
	   (propertize (format "(%.2fs)" elapsed)
		       'face (if (> elapsed 30) 'compilation-warning nil))
	   (unless nodir "in")
	   (unless nodir (propertize dir
				     'face 'dired-directory
				     'help-echo "SPC, mouse-1: Change to this directory"
				     'mouse-face 'highlight
				     'keymap jmm/eshell-hist-ex--hist-dir-keymap)))
	  (-non-nil)
	  (s-join " "))
     'input input)))

(defun jmm/maybe-duration-to-seconds (x)
  "Takes in X, which may be a string, in which case it is parsed with `timer-duration' and returns the duration in seconds.
If it isn't a string, it returns x (which could be a number in seconds, or nil)."
  (if (stringp x)
      (timer-duration x)
    x))

(defun eshell/dhist (&rest args)
  "Show history specific to this directory.
ARGS a list of optional flags."
  (eshell-eval-using-options
   "dhist" args
   '((?s "success" nil success "Only show successful commands")
     (?f "failed" nil failed "Only show failed commands")
     (?l "elapsed-lt" t elapsedlt "Only commands with an elapsed time less than that specified")
     (?g "elapsed-gt" t elapsedgt "Only commands with an elapsed time greater than that specified")
     (?h "help" nil nil "Output this help screen.")
     :preserve-args
     :usage "[-s]
Show the history of commands run in this specific directory.")
   (->> (jmm/eshell-directory-history)
	reverse
	(--remove (when elapsedgt (<= (plist-get it :elapsed) (jmm/maybe-duration-to-seconds elapsedgt))))
	(--remove (when elapsedlt (>= (plist-get it :elapsed) (jmm/maybe-duration-to-seconds elapsedlt))))
	(--remove (when failed (= (plist-get it :exit) 0)))
	(--remove (when success (/= (plist-get it :exit) 0)))
	(--map (jmm/eshell--format-expanded-history it t))
	(s-join "\n"))))

(defun eshell/xhist (&rest args)
  "Show extended history"
  (eshell-eval-using-options
   "xhist" args
   '((?s "success" nil success "Only show successful commands")
     (?f "failed" nil failed "Only show failed commands")
     (?l "elapsed-lt" t elapsedlt "Only commands with an elapsed time less than that specified")
     (?g "elapsed-gt" t elapsedgt "Only commands with an elapsed time greater than that specified")
     (?h "help" nil nil "Output this help screen.")
     :preserve-args
     :usage "[-s]")
   (->> (ring-elements jmm/eshell-expanded-history-ring)
	(-map #'cdr)
	reverse
	(--remove (when elapsedgt (<= (plist-get it :elapsed) (jmm/maybe-duration-to-seconds elapsedgt))))
	(--remove (when elapsedlt (>= (plist-get it :elapsed) (jmm/maybe-duration-to-seconds elapsedlt))))
	(--remove (when failed (= (plist-get it :exit) 0)))
	(--remove (when success (/= (plist-get it :exit) 0)))
	(--map (jmm/eshell--format-expanded-history it))
	(s-join "\n"))))

(defun jmm/eshell-insert-unquoted-text-as-input (str)
  "Take in some string set it to the Eshell prompt.
If the point isn't at the Eshell prompt, append to the end (with a separating space).
If the point is in the eshell prompt, insert at point.
Append it to the eshell prompt (if we're not at it), adding a space if necessary."
       (if (eshell-point-within-input-p (point))
	   (progn
	     (eshell-kill-input)
	     (insert str))
	 (save-excursion
	   (goto-char (point-max))
	   (jmm/eshell-insert-unquoted-text-as-input str))))

(defvar jmm/eshell-hist-ex-dhist-keymap
  (let ((mymap (make-sparse-keymap)))
    (define-key mymap (kbd "SPC") 'jmm/eshell-hist-ex-insert-input-at-point)
    (define-key mymap (kbd "<mouse-1>") 'jmm/eshell-hist-ex-input-insert-input-at-click)
    mymap))

(defvar jmm/eshell-hist-ex--hist-dir-keymap
  (let ((mymap (make-sparse-keymap)))
    (define-key mymap (kbd "SPC") 'jmm/eshell-hist-ex--cd-at-point)
    (define-key mymap (kbd "<mouse-1>") 'jmm/eshell-hist-ex--cd-at-click)
    mymap))

(defun jmm/eshell-hist-ex-input-insert-input-at-click (event)
  (interactive "e")
  (jmm/eshell-insert-unquoted-text-as-input
   (get-text-property (posn-point (event-end event)) 'input)))

(defun jmm/eshell-hist-ex-insert-input-at-point (point)
  (interactive "d")
  (jmm/eshell-insert-unquoted-text-as-input
   (get-text-property point 'input)))

(defun jmm/eshell-hist-ex--cd-at-click (event)
  (interactive "e")
  (goto-char (point-max))
  (eshell-kill-input)
  (insert (concat " cd " (eshell-quote-argument (get-text-property (posn-point (event-end event)) 'dir))))
  (eshell-send-input))

(defun jmm/eshell-hist-ex--cd-at-point (point)
  (interactive "d")
  (save-excursion
    (goto-char (point-max))
    (eshell-kill-input)
    (insert (concat " cd " (eshell-quote-argument (get-text-property point 'dir))))
    (eshell-send-input)))

;; (insert (jmm/eshell--format-expanded-history (cdar (ring-elements jmm/eshell-expanded-history-ring))))

;; Look at:
;; eshell-last-command-status

;;;;;;;;;; 

;;; Internal variables
(defvar-local jmm/eshell-hist-ex-dhist-ring nil)
(defvar-local jmm/eshell-hist-ex-dhist-index nil)

(defun jmm/eshell-hist-ex-previous-directory-input (arg)
  "Move backwards through directory history.
Kind of like `eshell-previous-matching-input-from-input' but without the matching.
Prefix determines how many to jump."
  (interactive "p")
  (if (not (memq last-command '(jmm/eshell-hist-ex-previous-directory-input
				jmm/eshell-hist-ex-next-directory-input)))
      ;; Starting a new search
      (setq jmm/eshell-hist-ex-dhist-ring (->> (jmm/eshell-directory-history)
					       (--map (plist-get it :input))
					       (ring-convert-sequence-to-ring))
	    jmm/eshell-hist-ex-dhist-index nil))
  (let ((pos (if (null jmm/eshell-hist-ex-dhist-index)
		 arg
	       (+ jmm/eshell-hist-ex-dhist-index arg))))
    (setq jmm/eshell-hist-ex-dhist-index pos)
    (delete-region eshell-last-output-end (point))
    (insert-and-inherit (ring-ref jmm/eshell-hist-ex-dhist-ring pos))))

(defun jmm/eshell-hist-ex-next-directory-input (arg)
  "Move forwards through directory history.
See `jmm/eshell-hist-ex-previous-directory-input'."
  (interactive "p")
  (jmm/eshell-hist-ex-previous-directory-input (- arg)))

(bind-key "S-<up>" #'jmm/eshell-hist-ex-previous-directory-input eshell-mode-map)
(bind-key "S-<down>" #'jmm/eshell-hist-ex-next-directory-input eshell-mode-map)

(provide 'em-hist-ex)
