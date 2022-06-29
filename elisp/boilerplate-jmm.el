;;; boilerplate-jmm.el --- Personal boilerplate for different programming languages  -*- lexical-binding: nil; -*-

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

(defvar boilerplate--directory nil
  "The root of a boilerplate project.")

(defvar boilerplate--git t
  "Whether or not we're using git.")

(defmacro boilerplate-save-excursion (&rest body)
  "Run BODY, saving window configuration, entering a recursive edit, and then reverting and redisplaying the starting window (usually a dired buffer)."
  (declare (indent 0) (debug t))
  `(progn
     (save-window-excursion
       (progn ,@body)
       (recursive-edit))
     (revert-buffer)
     (redisplay)))

(defmacro boilerplate--var (var &rest body)
  "See if VAR is bound, and return value. Otherwise run BODY.

This is usually used in skeletons.
"
  `(or (and (boundp ,var)
	    (stringp (symbol-value ,var))
	    (symbol-value ,var))
       (progn ,@body)))

(defmacro boilerplate--var-read (var prompt &optional initial)
  "See if VAR is bound, and return value. Otherwise prompt for the value and save it.
This is only used in skeletons."
  `(boilerplate--var ,var (set ,var (skeleton-read ,prompt ,initial))))

(defmacro boilerplate-maybe-skeleton (&rest body)
  "Possibly run BODY if buffer is empty."
  (declare (indent 0) (debug t))
  `(when (and (bobp) (eobp))
     ,@body))

(defun boilerplate--git-ignore (file)
  "Add FILE to .gitignore, if we're using git."
  (when boilerplate--git
    (vc-ignore file boilerplate--directory)))

(defun boilerplate--git-add (file)
  "Add FILE to git, if we're using git."
  (when boilerplate--git
    (let* ((myfile (if (file-name-absolute-p file)
		       (expand-file-name file)
		     (concat boilerplate--directory file))))
      (vc-register `(Git (,myfile) nil nil nil)))))

(defun boilerplate-file-skeleton (file gitignore skeleton)
  "Open up a FILE, possibly ignoring with GITIGNORE, and insert SKELETON"
  (progn
    (boilerplate-save-excursion
      (let ((auto-insert nil))
	(find-file (concat boilerplate--directory file))
	(when (and boilerplate--git gitignore)
	  (boilerplate--git-ignore file))
	(when skeleton
	  (boilerplate-maybe-skeleton
	    (funcall skeleton)))))
    (when (and boilerplate--git (not gitignore))
      (boilerplate--git-add file))))

(defmacro boilerplate-with-file (file gitignore &rest body)
  "Open up a FILE, possibly ignoring with GITIGNORE. And run BODY."
  (declare (indent 2) (debug t))
  (let ((myfile (make-symbol "file")))
    `(let ((,myfile ,file))
       (save-window-excursion
	 (let ((auto-insert nil))
	   (find-file (concat boilerplate--directory ,myfile))
	   (when (and boilerplate--git ,gitignore)
	     (boilerplate--git-ignore ,myfile))
	   ,@body))
       (revert-buffer)
       (redisplay)
       (when (and boilerplate--git (not ,gitignore))
	 (boilerplate--git-add ,myfile)))))


;;;;;;;;;;
;; R boilerplate

;;;###autoload
(defun boilerplate-r-nix ()
  "In the current directory, set up an R project with Nix and direnv.
Run this in a dired buffer of the new project."
  (interactive)
  (let ((boilerplate--directory default-directory)
	(boilerplate--git (y-or-n-p "Make git?"))
	mainfile)
    (when boilerplate--git
      (shell-command "git init"))
    (boilerplate-save-excursion
      (find-file (concat boilerplate--directory ".envrc"))
      (auto-insert)
      (set-buffer-modified-p t))
    (boilerplate--git-add ".envrc")
    (when (y-or-n-p "Trust env?")
      (shell-command (concat "direnv allow " boilerplate--directory))
      (envrc-reload))
    (boilerplate-file-skeleton "default.nix" nil #'nix-default-r-skeleton)
    (when (y-or-n-p "Build env?")
      (boilerplate-save-excursion
	(async-shell-command "nix build -vL -f . myenv -o myenv")))
    (boilerplate--git-ignore "myenv")
    (boilerplate--git-add ".gitignore")
    
    (setq mainfile (read-file-name "Main file: " default-directory))

    (boilerplate-save-excursion
      (find-file mainfile)
      (save-buffer)
      (ess-switch-process))
    (when boilerplate--git
      (boilerplate--git-add mainfile)
      (magit-status))))


;;;;;;;;;;
;; Shadow-CLJS boilerplate

;;;###autoload
(defun boilerplate-shadow-cljs ()
  "In the current dired directory, set up a shadow-cljs project with reagent."
  (interactive)
  (let* ((boilerplate--directory default-directory)
	 (projectname (read-from-minibuffer "Project namespace: " (file-name-base (directory-file-name boilerplate--directory))))
	 boilerplate--git boilerplate--port)
    (when (setq boilerplate--git (y-or-n-p "Make git?"))
      (shell-command "git init")
      (mapc #'boilerplate--git-ignore '("node_modules/" ".shadow-cljs/" "public/js/cljs-runtime/"
					"public/js/manifest.edn" "public/js/main.js")))
    (boilerplate-file-skeleton "shadow-cljs.edn" nil #'shadow-cljs-skeleton)
    ;; Make directories
    (progn
      (cl-loop for x in (list (concat "src/main/" projectname "/")
			      "src/dev/" "src/test/" "resources/" "public/css/" "public/js/")
	       do (make-directory (concat boilerplate--directory x) t))
      (revert-buffer)
      (redisplay))
    (boilerplate-file-skeleton "package.json" nil #'reagent-package-json-skeleton)
    (when (y-or-n-p "Install NPM packages?")
      (boilerplate-save-excursion
	(async-shell-command "npm install"))
      (boilerplate--git-add "package-lock.json"))
    (boilerplate-file-skeleton "public/index.html" nil #'html-shadow-cljs-skeleton)
    ;; TODO: This flickers too much because of the redisplay.
    (boilerplate-with-file ".dir-locals.el" nil
      (shadow-cljs-dir-locals-skeleton)
      (save-buffer))
    (boilerplate-file-skeleton "public/css/style.css" nil nil)
    (boilerplate-with-file (concat "src/main/" projectname "/core.cljs") nil
      ;; For some reason CIDER doesn't honor auto-insert? Or it's declared elsewhere.
      (erase-buffer)
      (reagent-core-cljs-skeleton)
      (save-buffer)
      (cider-jack-in-cljs nil)
      (recursive-edit))
    (boilerplate--git-add ".gitignore")
    ))

;;;###autoload (autoload 'shadow-cljs-dir-locals-skeleton "boilerplate-jmm")
(define-skeleton shadow-cljs-dir-locals-skeleton
  "So we don't prompt which build to make."
  ""
"((nil . ((cider-default-cljs-repl . shadow)
         (cider-shadow-default-options . \":app\"))))
"
  )

;;;###autoload (autoload 'reagent-core-cljs-skeleton "boilerplate-jmm")
(define-skeleton reagent-core-cljs-skeleton
  "Skeleton for main reagent core.cljs"
  ""
  "(ns "
  (boilerplate--var 'projectname "myproject")
  ".core
  (:require [clojure.core.async :as async :refer [go go-loop <! put!]]
            [cljs.core.async.interop :refer-macros [<p!]]
            [reagent.core :as r :refer-macros [with-let]]
            [reagent.dom :as rd]
            [reagent.ratom :as ra :refer-macros [run!]]
            [\"react\" :as react :refer [useRef useState]]
            [\"three\" :as THREE]
            [\"react-three-fiber\" :as r3f :refer [Canvas useFrame useUpdate]]))

(defonce myatom (r/atom \"foo\"))
(defn atom-input [value]
  [:input {:type \"text\"
           :value @value
           :on-change #(reset! value (-> % .-target .-value))}])

(defn my-view []
  [:div
   [:h1 \"Hello, world!\"]
   [:p \"What is your name? \" [atom-input myatom]]
   [:p \"Hello, \" @myatom \"!\"]])

(defn ^:dev/after-load start
  []
  (rd/render [my-view]
             (.getElementById js/document \"app\")))

(defn main! []
  (println \"[main]: loading\")
  (start))
")

;;;###autoload (autoload 'shadow-cljs-with-deps-skeleton "boilerplate-jmm" nil t nil)
(define-skeleton shadow-cljs-with-deps-skeleton
  "A skeleton for shadow-cljs.edn. Uses deps.edn."
  "This prompt is ignored."
";; shadow-cljs configuration
{ ;; This gets ignored?
 ;;     WARNING: The configured :source-paths in shadow-cljs.edn were ignored!
 ;;     When using :deps they must be configured in deps.edn
 #_#_ :source-paths
 [\"src/main\"
  \"src/test\"
  \"resources\"]

 :deps true
 :builds
 {:app {:target     :browser
        :output-dir \"resources/public/js\"
        :asset-path \"js\"
        #_#_:modules    {:main {:init-fn "
(if (boundp 'bp-projectname) (symbol-value 'bp-projectname) "myproject")
".core/init!}}
        #_#_:devtools   {:http-root \"resources/public\"
                         :http-port 8700}}}}
")

;;;###autoload (autoload 'shadow-cljs-skeleton "boilerplate-jmm")
(define-skeleton shadow-cljs-skeleton
  "A skeleton for shadow-cljs.edn reagent."
  ""
"{:source-paths
 [\"src/dev\"
  \"src/main\"
  \"src/test\"
  \"resources\"]

 :dependencies
 [[reagent \"1.0.0-alpha2\"]
  [re-frame \"1.1.1\"]
  [org.clojure/core.async \"1.3.610\"]]

 :dev-http {"
(boilerplate--var-read 'boilerplate--port "Port: " "8080")
" \"public\"}

 :builds
 {:app {:target     :browser
        :output-dir \"public/js\"
        :asset-path \"js\"
        :modules    {:main {:init-fn "
(boilerplate--var 'projectname "myproject")
	".core/main!}}
        #_#_:devtools   {:http-root \"public\"
                         :http-port 8700}}}}")

;;;###autoload (autoload 'reagent-package-json-skeleton "boilerplate-jmm")
(define-skeleton reagent-package-json-skeleton
  ;; Never mind
  "Package.json skeleton for reagent"
  ""
"{
  \"name\": \""
(boilerplate--var 'projectname "myproject")
"\",
  \"version\": \"0.0.1\",
  \"private\": true,
  \"devDependencies\": {
    \"shadow-cljs\": \"2.11.0\"
  },
  \"dependencies\": {
    \"react\": \"^16.13.1\",
    \"react-dom\": \"^16.13.1\",
    \"react-three-fiber\": \"^4.2.20\",
    \"three\": \"^0.125.0\"
  }
}")

;;;###autoload (autoload 'html-shadow-cljs-skeleton "boilerplate-jmm")
(define-skeleton html-shadow-cljs-skeleton
  "A simple skeleton for HTML stuff for reagent."
  ""
"<!DOCTYPE html>
<html>
    <head>
	<meta charset=\"UTF-8\">
	<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
	<title>"
(skeleton-read "Title: ")
"</title>
	<link async rel=\"stylesheet\" href=\"css/style.css\" />
    </head>
    <body>
	<noscript>You need to enable JavaScript to run this app.</noscript>
	<div id=\"app\">The app should appear here when loaded.</div>
	<script src=\"js/main.js\" type=\"text/javascript\"></script>
    </body>
</html>
")

(provide 'boilerplate-jmm)
;;; boilerplate-jmm.el ends here
