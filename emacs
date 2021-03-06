;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; IMPORTANT: For Emacs >= 23.2, you must place this *before* any
;; CEDET component (including EIEIO) gets activated by another 
;; package (Gnus, auth-source, ...).
(load-file "/usr/src/cedet-1.1/common/cedet.el")

;; Enable EDE (Project Management) features
(global-ede-mode 1)

;; Enable EDE for a pre-existing C++ project
;; (ede-cpp-root-project "NAME" :file "~/myproject/Makefile")


;; Enabling Semantic (code-parsing, smart completion) features
;; Select one of the following:

;; * This enables the database and idle reparse engines
(semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode,
;;   imenu support, and the semantic navigator
(semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as intellisense mode,
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;; (semantic-load-enable-gaudy-code-helpers)

;; * This enables the use of Exuberant ctags if you have it installed.
;;   If you use C++ templates or boost, you should NOT enable it.
;; (semantic-load-enable-all-exuberent-ctags-support)
;;   Or, use one of these two types of support.
;;   Add support for new languages only via ctags.
;; (semantic-load-enable-primary-exuberent-ctags-support)
;;   Add support for using ctags as a backup parser.
;; (semantic-load-enable-secondary-exuberent-ctags-support)

;; Enable SRecode (Template management) minor-mode.
;; (global-srecode-minor-mode 1)


;;fun
;;(global-set-key (kbd "C-x C-s") 'spook)


;; inital scratch text
(setq initial-scratch-message "--== Scratchpad ==--

")


(add-to-list 'load-path "~/.emacs.d")
(load "php-mode")
(load "lorem-ipsum.el")
(load "linum.el")
(load "word-count.el")

;; cedet
;(load-file "~/.elisp/cedet/common/cedet.el")
;(global-ede-mode 1)                      ; Enable the Project management system
;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
;(global-srecode-minor-mode 1)            ; Enable template insertion menu
;(require 'semantic-ia)
;(require 'semantic-gcc)


;; lorem ipsum
(autoload 'Lorem-ipsum-insert-paragraphs "lorem-ipsum" "" t)
(autoload 'Lorem-ipsum-insert-sentences "lorem-ipsum" "" t)
(autoload 'Lorem-ipsum-insert-list "lorem-ipsum" "" t)


;; auto tag creation
;(defun create-tags (dir-name)
;  "Create tags file."
;  (interactive "DDirectory: ")
;  (eshell-command 
;   (format "find %s -type f -name '*.[ch]' -o -name '*.cpp' | etags -L -" dir-name)))


;; syntax highlighting
(global-font-lock-mode 't)
(setq font-lock-maximum-decoration 't)
(setq font-lock-maximum-size '262144)

;; show me line and column nos
(line-number-mode 't)
(column-number-mode 't)
;; show current function too
;;(show-func-mode t)

;; none of these please
(scroll-bar-mode '0)
(tool-bar-mode '0)
(menu-bar-mode '0)

;; save and restore my buffers every time
;;(desktop-save-mode 1)


;; c mode customizations
(cwarn-mode t)
(setq c-default-style "linux")
(which-func-mode t)
(setq c-basic-offset 4)
;;(setq default-tab-width 2)
;;(hide-ifdefs t)

;; Show Date And time in mode-line
(setq display-time-day-and-date t )
(setq display-time-24hr-format t)
(display-time)

;; line numbers
(global-linum-mode 1)

;; make it easy on eyes ...
(set-foreground-color "gray")
(set-background-color "black")

(set-face-foreground 'default "gray")
(set-face-background 'default "black")

; window modifications
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; scrolling
(setq
 scroll-margin 5 ;; when to start scrolling
 scroll-conservatively 100000 
 scroll-preserve-screen-position 1)

(defun scroll-down-keep-cursor ()
   ;; Scroll the text one line down while keeping the cursor
   (interactive)
   (scroll-down 1))

(defun scroll-up-keep-cursor ()
   ;; Scroll the text one line up while keeping the cursor
   (interactive)
   (scroll-up 1)) 

(global-set-key (kbd "C-,") 'scroll-down-keep-cursor)
(global-set-key (kbd "C-.") 'scroll-up-keep-cursor) 

;; scrolling mouse
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; two lines at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; printer command
(setq lpr-command "lp")

;; Iteractively Do Things mode
(require 'ido)
(ido-mode t)

;; auto complete
(add-to-list 'load-path "~/.emacs.d/auto-complete-1.3.1")
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;; file column indicator
(add-to-list 'load-path "~/.emacs.d/fill-column-indicator.el")
(require 'fill-column-indicator)
(define-globalized-minor-mode
  global-fci-mode fci-mode (lambda() (fci-mode 1)))
(global-fci-mode 0)
(setq fci-rule-column '80)                                              
(setq fci-rule-width 1)
(setq fci-rule-color "darkgrey")

;; highlight the current line
;;(setq hl-line-face 'hl-line)
(global-hl-line-mode '0)
;;(setq highlight-current-line-globally t)
;;(setq highlight-current-line-high-faces nil)
;;(setq highlight-current-line-whole-line nil)
;;(setq hl-line-face (quote highlight))

; highlight parentheses when the cursor is next to them
(require 'paren)
(show-paren-mode t)

;; custom keyboard shortcuts
(global-set-key (kbd "C-c m") 'compile)

;; matlab
(add-to-list 'load-path "~/.emacs.d/matlab_emacs.d")
(require 'matlab-load)

;; ya-snippet
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;; subwor mode (camelcase mode)
(global-subword-mode 1)

;; i hate tabs!
(setq-default indent-tabs-mode nil)

;; python mode
;;(add-to-list 'load-path "~/.emacs.d/python-mode.el-6.1.1") 
;;(setq py-install-directory "~/.emacs.d/python-mode.el-6.1.1")
;;(require 'python-mode)
(add-hook 'python-mode-hook
      (lambda ()
        (setq indent-tabs-mode nil)
        (setq tab-width 4)
        (setq python-indent 4)))

;; ipython
;;(setq py-shell-name "ipython")
;;(require 'ipython)

;; mail mode
(server-start)
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))


