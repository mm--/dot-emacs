(deftheme jmm-default
  "A default theme for storing settings.
You'll probably load other themes on top of this.")

(custom-theme-set-faces
 'jmm-default
 '(default ((t (:family "Fira Code")))))
 
(custom-theme-set-variables
 'jmm-default
 '(tool-bar-mode nil nil nil "I don't really use the tool bar")
 '(visible-bell 0)
 '(ring-bell-function 'ignore))

(provide-theme 'jmm-default)
