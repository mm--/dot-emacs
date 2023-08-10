(deftheme jmm-default
  "A default theme for storing settings.
You'll probably load other themes on top of this.")

;; (custom-theme-set-faces
;;  'jmm-default
;;  '(default ((t (:family "Fira Code")))))

;; TODO: Don't add some settings if `noninteractive' is t

;; TODO: Some settings shouldn't be enabled in some "basic" setting
;; where I'm showing someone how to use Emacs.  Like, I should
;; normally keep `scroll-bar-mode' turned on to show newcomers.

;; TODO: Can we defer some settings to prevent loading?

(custom-theme-set-variables
 'jmm-default
 ;; I don't think the "comment" field is actually really used at all for non-user themes.
 '(visible-bell 0 nil nil "Set visible bell on")
 '(ring-bell-function 'ignore nil nil "Visible bell")
 '(enable-recursive-minibuffers t nil nil "Sometimes I like to pipe the output of `shell-command' into `find-file'")
 ;; Mark settings
 '(transient-mark-mode nil nil nil "You know, I think I prefer using the mark just to remember where I was.")
 '(jmm-mark-mode t)
 '(set-mark-command-repeat-pop t)
 '(mark-ring-max 4 nil nil "Setting a low default for the mark ring makes it easier to loop around.")
 ;; Killing/yanking
 '(kill-read-only-ok t nil nil "You can set the buffer read only temporarily to copy a region without killing it.")
 '(copy-region-blink-delay 0.1 nil nil "The default 1 second is too long and kinda confuses me.")
 ; Maybe set `yank-pop-change-selection'?
 ;; Some modes to turn on
 '(column-number-mode t)
 '(show-paren-mode t)
 '(menu-bar-mode t nil nil "The menu is actually helpful for finding commands and displaying context.")
 '(tool-bar-mode nil nil nil "I don't really use the tool bar")
 '(scroll-bar-mode nil nil nil "Historically I haven't used the scroll bar as it took too much horizontal space, especially with many windows.")
 '(horizontal-scroll-bar-mode nil)
 '(blink-cursor-mode nil nil nil "I tended to find cursor blinking distracting.")
 '(pixel-scroll-precision-mode t nil nil "Makes scrolling a lot nicer with the mouse wheel.")
 ;; Other display settings
 '(composition-break-at-point nil nil nil "Default. Doesn’t split things like COMBINING ACUTE ACCENT.")
 '(display-raw-bytes-as-hex t nil nil "I don’t open binary files often, but I probably understand hex better than octal.")
 '(x-stretch-cursor t nil nil "Might be helpful to see tabs.")
 '(show-trailing-whitespace t nil nil "I often litter files with trailing whitespace. This might help me spot it beforehand.")
 '(indicate-empty-lines t nil nil "Maybe make it easier to spot trailing newlines.")
 '(what-cursor-show-names t nil nil "Often I’m using C-u C-x = to see the name of the character.  This way I don’t need an extra help buffer to pop up just to see the name.")
 ;; Mode line appearance
 ;; I just get rid of `global-mode-string' since I want that shown in the tab bar.
 '(mode-line-misc-info '((which-function-mode (which-func-mode ("" which-func-format " ")))))
 '(mode-line-compact nil nil nil "Warning, setting this to 'long messes up the separation of minor modes when hovering over them with a mouse.")
 ;; Tab bar
 '(tab-bar-format
   '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator tab-bar-format-align-right tab-bar-format-global)) ;; `tab-bar-format-global' seems interesting
 '(tab-bar-mode 1 nil nil "I kind of like tab bar.  I use it for what I used to use \"eyebrowse\" for.")
 '(tab-bar-new-tab-to 'rightmost)
 '(tab-bar-select-tab-modifiers '(control meta))
 '(tab-bar-show t nil nil "Always show the tab bar since I'm using `tab-bar-format-global'.")
 '(tab-bar-tab-hints t)
 '(tab-bar-tab-name-function 'tab-bar-tab-name-truncated)
 '(tab-bar-tab-name-truncated-max 10)
 ;; Completions
 '(completion-styles '(basic partial-completion emacs22 initials) nil nil "Some better completions for M-x and find-file. I particularly like \"initials\".")
 '(completion-cycle-threshold nil nil nil "TODO: I need to figure out a way to signal that other completions exist")
 '(completion-auto-select 'second-tab nil nil "Automatically jump to *Completions* buffer if I press tab twice.")
 '(completions-group t nil nil "Grouping seems nice.  Tried it with C-x 8 RET.")
 '(completions-sort nil nil nil "The default alphabetical sort mechanism fucks up viewing buffers by recency.")
 '(completion-auto-wrap nil nil nil "Don't immediately wrap.  May make it easier to go to the first candidate.")
 ;; Bookmarks
 '(bookmark-fontify nil)
 '(bookmark-save-flag 1)
 '(bookmark-set-fringe-mark nil)
 '(bookmark-watch-bookmark-file 'silent)
 ;; Mouse settings
 '(mouse-yank-at-point t)
 '(context-menu-mode 1 nil nil "Make right click open a context menu. (Which you can otherwise access with S-<f10>). I don't really use `mouse-save-then-kill'.")
 '(focus-follows-mouse t nil nil "Usually my window manager uses focus-follows-mouse.")
 ;; '(mouse-autoselect-window nil nil nil "I used to use sloppy focus, but now I don't.")
 '(mouse-autoselect-window -0.05 nil nil "Sloppy focus, but waits for the mouse to stop.")
 '(xterm-mouse-mode 1 nil nil "Allow people to click around if using the terminal.")
 ;; Dictionary
 '(dictionary-server "dict.org" nil nil "I haven’t yet installed a dictionary locally.  This might be a bad idea if the dict protocol doesn’t use encryption.")
 '(dictionary-use-single-buffer t)
 ;; isearch
 '(isearch-allow-motion t nil nil "Being able to press M-< instead of M-s M-< is helpful")
 '(isearch-lazy-count t nil nil "I like knowing whether or not I'm at the first match.")
 ;; Repeat mode
 '(repeat-exit-key "<return>") ;; Not sure if I actually like this.
 '(repeat-mode t)
 ;; Grep and ripgrep
 '(xref-search-program (if (executable-find "rg") 'ripgrep 'grep))
 '(grep-command (when (executable-find "rg") "rg --no-heading --color=auto -nH --null -L --no-messages -z -e "))
 ;; Hmm.... actually it looks like ripgrep doesn't support "--exclude"
 ;; '(grep-template "rg --no-heading <X> <C> -nH -z --null -L -e <R> <F>")'
 '(grep-find-template (when (executable-find "rg") "find -H <D> <X> -type f <F> -exec rg --no-heading <C> -nH -z --null -e <R> \{\} +"))
 '(grep-use-null-device (when (executable-find "rg") nil 'auto-detect))

 )

(provide-theme 'jmm-default)
