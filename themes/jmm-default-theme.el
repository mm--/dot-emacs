(deftheme jmm-default
  "A default theme for storing settings.
You'll probably load other themes on top of this.")

;; (custom-theme-set-faces
;;  'jmm-default
;;  '(default ((t (:family "Fira Code")))))
 
(custom-theme-set-variables
 'jmm-default
 ;; I don't think the "comment" field is actually really used at all.
 '(tool-bar-mode nil nil nil "I don't really use the tool bar")
 '(visible-bell 0)
 '(ring-bell-function 'ignore)
 ;; I kind of like tab bar.  I use it for what I used to use "eyebrowse" for.
 '(tab-bar-select-tab-modifiers '(control meta))
 '(tab-bar-tab-hints t)
 '(tab-bar-show 1)
 '(tab-bar-tab-name-function 'tab-bar-tab-name-truncated)
 '(tab-bar-tab-name-truncated-max 10)
 '(tab-bar-new-tab-to 'rightmost)
 '(tab-bar-mode 1)
 )

(provide-theme 'jmm-default)
