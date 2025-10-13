;; .emacs


;; Path for various modules
(add-to-list 'load-path "~/1data/tehnic/info/emacs/packages")

;; Path for various modules
(add-to-list 'load-path "/Volumes/Data/1data/tehnic/info/emacs/packages")

;; Path for various modules
(add-to-list 'load-path "~/v/4sunray/settings/emacs/packages")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Useful settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Windows-like clipoard shortcuts
(cua-mode t)

;; Set initial frame geometry
(if (window-system)
    (set-frame-height (selected-frame) 47))
(if (window-system)
    (set-frame-width (selected-frame) 120))

;; "open with" in same frame rather than in new frames (Emacs 23.3+)
(setq ns-pop-up-frames nil)

;; Set other frames parameters
(setq default-frame-alist
      '(
	(height . 45)
	(witdh . 120)
	;; (foreground-color . "yellow")
	;; (background-color . "black")
	;; (font . "Monospace-10")
	)
      )


;; no startup screen
(setq inhibit-startup-screen t)

;; Uniquify: 'post-forward' puts directories in forward order
;; after the file name, as in `file|top/middle'.
(setq 
 uniquify-buffer-name-style 'post-forward
 uniquify-separator "|")

(defun kill-current-buffer ()
  "Kill the current buffer, without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))

(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-x C-c") 'ask-before-closing)

;; Change cursor appearance according to mode; from
;; http://emacs-fu.blogspot.com/2009/12/changing-cursor-color-and-shape.html
;; Inspired by http://www.emacswiki.org/emacs/ChangingCursorDynamically
(defun djcb-set-cursor-according-to-mode ()
  "change cursor color and type according to some minor modes."
  (cond
   (buffer-read-only
    (set-cursor-color "darkgray")
    (setq cursor-type 'box))
   (overwrite-mode
    (set-cursor-color "red")
    (setq cursor-type 'hbar))
   (t 
    (set-cursor-color "black")
    (setq cursor-type 'bar))))

(add-hook 'post-command-hook 'djcb-set-cursor-according-to-mode)

;; delete selection on text enter
(unless delete-selection-mode
  (delete-selection-mode))

;; cursor blinking
(setq blink-cursor-interval 0.18)
(unless blink-cursor-mode
  (blink-cursor-mode))

;; mouse wheel scroll amount
(setq mouse-wheel-scroll-amount '(3 ((shift) . 7) ((control) . 1)))

;; do not beep
(setq visible-bell t)

;; backup files handling (may save you from disasters)
(setq backup-by-copying t)
(setq backup-directory-alist `(("." . "/w/2local/backup/saves")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; save mini-buffer history
(savehist-mode 1)

;; remember cursor positions
(require 'saveplace)
(setq-default save-place t)

;; enable syntax highlighting
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; better frame titles
(setq frame-title-format
      (concat  "%b - emacs"))

(setq-default frame-title-format
              '(:eval
                (format "%s@%s: %s %s"
                        (or (file-remote-p default-directory 'user)
                            user-real-login-name)
                        (or (file-remote-p default-directory 'host)
                            system-name)
                        (buffer-name)
                        (cond 
                         (buffer-file-truename
                          (concat "(" buffer-file-truename ")"))
                         (dired-directory
                          (concat "{" dired-directory "}"))
                         (t
                          "[no file]")))))

;; enable copy/paste with system clipboard
(setq x-select-enable-clipboard t)

;; show column
(setq column-number-mode t)

;; highlight matching paren...
(show-paren-mode t)

;; enable autoindent by default
(global-set-key (kbd "RET") 'newline-and-indent)

;; Scrollbar on the right
(setq scroll-bar-mode-explicit t)
(set-scroll-bar-mode `right)

;; PgUp/PgDn return you to the same line where you left from
(setq scroll-preserve-screen-position t)

;; scroll one line at a time
(defun scroll-one-line-up (&optional arg)
  "Scroll the selected window up (forward in the text) one line (or N lines)."
  (interactive "p")
  (scroll-up (or arg 1)))
(defun scroll-one-line-down (&optional arg)
  "Scroll the selected window down (backward in the text) one line (or N)."
  (interactive "p")
  (scroll-down (or arg 1)))

;; isearch: find word under cursor with regex by default
(global-set-key (kbd "C-s") 'my-isearch-word-at-point)

;; better grep command
(setq grep-find-command "find . -regextype posix-extended -regex '.*' -exec grep -HnPi '' {} \\;")
(setq grep-command "grep -nHPi ")
(setq find-name-arg "-regextype posix-extended -regex ")

;; hide menu and tool bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; better buffer switching
(unless iswitchb-mode (iswitchb-mode))

;; default to unified diffs
(setq diff-switches "-u")

;; ediff: no frames
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; ediff: side-by-side comparison
(setq ediff-split-window-function 'split-window-horizontally)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "#2a2a2a" :foreground "#b8b8b8" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "unknown" :family "Ubuntu Mono")))))

(set-face-attribute 'default nil :font "DejaVu Sans Mono-10")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Extra keyboard shortcuts
;; NOTE: S-fX doesn't work from VNC viewers and terminals
;; NOTE: C-fX doesn't work from terminals
;; NOTE: f1-f4 don't work from terminals
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; <f2-...> is originally mapped to functions like 2C-two-columns
;; <f3> is originally mapped to kmacro-start-macro-or-insert-counter
;; <f4> is originally mapped to kmacro-end-or-call-macro
;; C-x C-f is originally mapped to find-file
;; C-x b is originally mapped to switch-to-buffer
;; C-x C-b is originally mapped to list-buffers
;; C-x d is originally mapped to dired
;; C-x k is originally mapped to kill-buffer

;;
;; Changes or additions to standard Emacs combinations
;;

;; isearch: Press the keys below to fill the symbol at point
(define-key isearch-mode-map (kbd "C-e") 'isearch-yank-symbol)

;; isearch: Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; isearch: Activate occur easily
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

;; scroll without moving the cursor
(global-set-key (kbd "M-p") 'scroll-one-line-down)
(global-set-key (kbd "M-n") 'scroll-one-line-up)

;; buffer menu in current window
(global-set-key (kbd "C-x C-b") 'buffer-menu)

;; kill current buffer directly rather than asking for the buffer to kill
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; Quick results movement
(global-set-key (kbd "<f6>") 'next-error)
(global-set-key (kbd "<f7>") 'previous-error)
(global-set-key (kbd "<f11>") 'cscope-next-symbol)
(global-set-key (kbd "<f12>") 'cscope-prev-symbol)

;; Quick revert
(global-set-key (kbd "<f5>") 'revert-buffer)

;; Other bindings
(global-set-key (kbd "C-c c") 'compile)
(global-set-key (kbd "C-c g") 'grep)
(global-set-key (kbd "C-c i") 'find-grep)
(global-set-key (kbd "C-c d") 'find-name-dired)
(global-set-key (kbd "C-c f") '(lambda () (interactive) (fci-mode) (auto-fill-mode)))

;; Other combinations, hope to work from VNC, too
;; From the terminal, use the "f9" combinations.
(global-set-key (kbd "<f7>") 'switch-to-buffer)
(global-set-key (kbd "<C-f7>") 'find-file)
(global-set-key (kbd "<f6>") 'next-multiframe-window)
(global-set-key (kbd "<C-f6>") 'previous-multiframe-window)
(global-set-key (kbd "<C-f4>") 'kill-current-buffer)
(global-set-key (kbd "C-.") 'next-error)
(global-set-key (kbd "C-,") 'previous-error)
(global-set-key (kbd "C-'") 'cscope-next-symbol)
(global-set-key (kbd "C-;") 'cscope-prev-symbol)
(global-set-key (kbd "<f11>") 'cscope-find-this-symbol)
(global-set-key (kbd "<C-f11>") 'cscope-find-this-file)
(global-set-key (kbd "<f12>") 'cscope-find-global-definition)
(global-set-key (kbd "<C-f1>") 'man)

;; <f9> combinations (should work in VNC and terminals)
(global-set-key (kbd "<f9> c") 'compile)
(global-set-key (kbd "<f9> g") 'grep)
(global-set-key (kbd "<f9> i") 'grep-find)
(global-set-key (kbd "<f9> d") 'cscope-find-global-definition)
(global-set-key (kbd "<f9> s") 'cscope-find-this-symbol)
(global-set-key (kbd "<f9> f") 'cscope-find-this-file)
(global-set-key (kbd "<f9> e") 'find-name-dired)
(global-set-key (kbd "<f9> h") 'man)
(global-set-key (kbd "<f9> .") 'next-error)
(global-set-key (kbd "<f9> ,") 'previous-error)
(global-set-key (kbd "<f9> >") 'cscope-next-symbol)
(global-set-key (kbd "<f9> <") 'cscope-prev-symbol)
(global-set-key (kbd "<f9> b") 'af-bookmark-toggle)
(global-set-key (kbd "<f9> ]") 'af-bookmark-cycle-forward)
(global-set-key (kbd "<f9> [") 'af-bookmark-cycle-reverse)
(global-set-key (kbd "<f9> B") 'af-bookmark-clear-all)
(global-set-key (kbd "<f9> r") 'revert-buffer)

;; for the Sun keyboards
(global-set-key (kbd "<cancel>") 'keyboard-escape-quit)
(global-set-key (kbd "<SunFront>") 'save-buffer)
(global-set-key (kbd "<SunOpen>") 'find-file)
(global-set-key (kbd "<XF86Cut>") 'kill-region)
(global-set-key (kbd "<XF86Copy>") 'kill-ring-save)
(global-set-key (kbd "<XF86Paste>") 'yank)

;; more key bindings
(global-set-key (kbd "<f2>") 'save-buffer)
(global-set-key (kbd "<S-f2>") 'revert-buffer)
(global-set-key (kbd "<M-f2>") 'revert-buffer)
(global-set-key (kbd "ESC <f2>") 'revert-buffer)
(global-set-key (kbd "<f5>") 'next-error)
(global-set-key (kbd "<S-f5>") 'previous-error)
(global-set-key (kbd "<f6>") 'next-multiframe-window)
(global-set-key (kbd "<S-f6>") 'previous-multiframe-window)
(global-set-key (kbd "C-6") 'switch-to-buffer)
(global-set-key (kbd "C-^") 'switch-to-buffer)
(global-set-key (kbd "<f7>") 'compile)
(global-set-key (kbd "<f9>") 'grep)
(global-set-key (kbd "<f11>") 'find-tag)
(global-set-key (kbd "<S-f11>") 'find-tag-other-window)
;; (global-set-key (kbd "<S-f11>") 'menu-bar-next-tag)
(global-set-key (kbd "<f12>") 'tags-apropos)
(global-set-key (kbd "<S-f12>") 'find-tag-regexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Maybe-useful settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Modules
(require 'highlight-parentheses) ; highlighting matching paren
(require 'bm) ; quick bookmarks - bm
(require 'highlight-symbol) ; highlighting symbol under cursor
(require 'grep-a-lot) ; multiple grep buffers
;; (require 'color-themes-sorel) ; custom color schemes
(require 'etags-integration) ; etags
(require 'frame-config) ; rember frame configuration

;; Special buffers with their own windows
(setq special-display-buffer-names
     `(
       ("*grep*" . ((name . "*grep*")
                    ,@default-frame-alist))
       ("*compilation*" . ((name . "*compilation*")
                           ,@default-frame-alist))
       ))

;; Close buffer with frame
(add-hook 'delete-frame-functions
          (lambda (frame)
            (let* ((window (frame-selected-window frame))
                   (buffer (and window (window-buffer window))))
              (when (and buffer (buffer-file-name buffer))
                (kill-buffer buffer)))))


;; don't split windows horizontally
(setq split-height-threshold 60) ; split vertically if it has at least n columns
(setq split-width-threshold nil)

;; for smooth scrolling and disabling the automatical recentering of emacs when moving the cursor
(setq scroll-margin 1
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)

;; uncomment this line to disable loading of "default.el" at startup
(setq inhibit-default-init t)

;; comment this line to enable visual feedback on selections
(setq transient-mark-mode t)

;; highlight parantheses tree globally
(define-globalized-minor-mode global-highlight-parentheses-mode
  highlight-parentheses-mode
  (lambda ()
    (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

;; Some grep-find-command alternatives
;; (setq grep-find-command "find . -type f -print0 | xargs -0 -e grep -niHP ")
;; (setq grep-find-command "fif -d . -f '.*\\.(.*)' '\\bTEXT\\b'")
;; (setq grep-find-command "find . -regextype posix-extended -regex '.*' -type f -print0 | xargs -0 -e grep -nPi '\\bTEXT\\b' | awk 'END { print NR, \"matches.\" } 1'")

;; Dired mode...
(put 'dired-find-alternate-file 'disabled nil)
(add-hook 'dired-mode-hook
	  (lambda ()
	    (define-key dired-mode-map (kbd "<RET>")
	      'dired-find-alternate-file) ; was dired-advertised-find-file
	    (define-key dired-mode-map (kbd "^")
	      (lambda () (interactive) (find-alternate-file ".."))) ; was dired-up-directory
	    ))
;; When having dired of different dir in 2 panes, and when you press C
;; to copy, emacs will automatically prompt the other dir path
(setq dired-dwim-target t)

(defun* get-closest-pathname (&optional (file "Makefile"))
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
This may not do the correct thing in presence of links. If it
does not find FILE, then it shall return the name of FILE in the
current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) ; the win32 builds should translate this correctly
    (expand-file-name ;file
     (loop 
      for d = default-directory then (expand-file-name ".." d)
      if (file-exists-p (expand-file-name file d))
      return d
      if (equal d root)
      return "."))))
;; To use, add this to c-mode-hook and c++-mode-hook
;; (setq compile-command (format "cd %s && make -k " (get-closest-pathname)))

;; always highlight symbol under cursor
(add-hook 'find-file-hook 'highlight-symbol-mode)

;; Setup keys for multiple grep buffers
(grep-a-lot-setup-keys)

;; Expand/shrink current window height/width
(defun expand-window-height ()
  "Expand current window to use one more fraction of the other window's height."
  (interactive)
  (enlarge-window (round (/ (window-height (next-window)) 2.5))))
(defun shrink-window-height ()
  "Shrink current window to use one more fraction of the other window's height."
  (interactive)
  (shrink-window (round (/ (window-height (next-window)) 3))))
(defun expand-window-width ()
  "Expand current window to use one more fraction of the other window's width."
  (interactive)
  (enlarge-window-horizontally (round (/ (window-width (next-window)) 3))))
(defun shrink-window-width ()
  "Shrink current window to use one more fraction of the other window's width."
  (interactive)
  (shrink-window-horizontally (round (/ (window-height (next-window)) 3))))

;; bm bookmarks options
(setq bm-highlight-style 'bm-highlight-only-fringe)

;; highlight-symbol options
(setq highlight-symbol-idle-delay '0.5)
;; (setq highlight-symbol-on-navigation-p 't)

;; ReST mode for TXT
(setq auto-mode-alist
      (append '(("\\.txt$" . rst-mode)
                ("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))

;;
;; Currently unused keyboard shortcuts
;;

(global-set-key (kbd "C-x C-f") 'find-file-other-frame)
(global-set-key (kbd "C-x b") 'switch-to-buffer-other-frame)
(global-set-key (kbd "C-x d") 'dired-other-frame)

(global-set-key (kbd "<f9> m s") 'kmacro-start-macro-or-insert-counter)
(global-set-key (kbd "<f9> m e") 'kmacro-end-or-call-macro)
(global-set-key (kbd "<f9> v") 'expand-window-height)
(global-set-key (kbd "<f9> V") 'shrink-window-height)
(global-set-key (kbd "<f9> h") 'expand-window-width)
(global-set-key (kbd "<f9> H") 'shrink-window-width)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Programmer settings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Modules
(require 'bookmarks-anthonyf) ; quick bookmarks - bookmarks-anthonyf
(require 'cl)                 ; compilation setup
(require 'find-word)          ; find word at point
(require 'compile)
(require 'xcscope)            ; CScope
(require 'uniquify)           ; Uniquify file names better than the default.
(require 'web-mode)           ; mode for editing Web files
(require 'find-word)
(require 'gud)                ; Apple's GUD for LLDB

;; cscope: disable arrow overlays which are annoing when running from a terminal
(setq cscope-allow-arrow-overlays nil)

;; automatic formatting on paste
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
	   (and (not current-prefix-arg)
		(member major-mode '(emacs-lisp-mode lisp-mode
						     clojure-mode    scheme-mode
						     haskell-mode    ruby-mode
						     rspec-mode      python-mode
						     c-mode          c++-mode
						     objc-mode       latex-mode
						     perl-mode       tcl-mode
						     plain-tex-mode))
		(let ((mark-even-if-inactive transient-mark-mode))
		  (indent-region (region-beginning) (region-end) nil))))))

(setq compile-command "make ")

;; automatic scrolling in compilation
(setq compilation-scroll-output t)

;; TRPZ C style
(defconst trpz-c-style
  '((c-hanging-braces-alist . ((substatement-open after)
			       (brace-list-open)))
    )
  "TRPZ C Programming Style")
(c-add-style "TRPZ" trpz-c-style)

;; Settings for all languages
(defun my-prog-common-hook ()
  ;; (highlight-parentheses-mode t)
  ;; (linum-mode)
  ;; (flyspell-prog-mode)
  ;; disable all electric keys for all modes
  (local-unset-key (kbd ")"))
  (local-unset-key (kbd "("))
  (local-unset-key (kbd "["))
  (local-unset-key (kbd "]"))
  (local-unset-key (kbd "{"))
  (local-unset-key (kbd "}"))
  (local-unset-key (kbd "#"))
  (local-unset-key (kbd "*"))
  (local-unset-key (kbd ","))
  (local-unset-key (kbd ":"))
  (local-unset-key (kbd ";"))
  (local-unset-key (kbd "/"))
  (local-unset-key (kbd "C-m"))
  (setq post-self-insert-hook nil) ; disable all electric characters (Emacs 24+)
  )

;; C++ and C common mode
(defun my-c-common-func ()
  (c-set-offset 'substatement '+)         ;  1 * c-basic-offset
  (c-set-offset 'statement-case-intro '+) ;  
  (c-set-offset 'statement-case-open '+)  ;  
  (c-set-offset 'topmost-intro '0)        ; -1 * c-basic-offset
  (c-set-offset 'access-label '--)        ; -2 * c-basic-offset
  (c-set-offset 'inline-open '-)          ; -1 * c-basic-offset
  (c-set-offset 'case-label 0)            ;  0
  (c-set-offset 'substatement-open '0)
  (c-set-offset 'statement-block-intro '+)
  (c-set-offset 'arglist-cont-nonempty '+) ; multiline function calls
  (setq indent-tabs-mode nil) ;; force only spaces for indentation
  (my-prog-common-hook)
  ;; (c++-toggle-auto-hungry-state 1)
  ;; (setq c++-delete-function 'backward-delete-char)
  )

;; C++ mode
(defun my-c++-mode-hook ()
  ;; (define-key c++-mode-map "\C-m" 'newline-and-indent)
  (define-key c++-mode-map "\C-ce" 'c-comment-edit)
  (setq c++-tab-always-indent t)
  (c-set-style "TRPZ")
  (setq tab-width 8)
  (setq c-indent-level 4)
  (setq c-basic-offset 4)
  (my-c-common-func))

;; C mode
(defun my-c-mode-hook ()
  ;; (define-key c-mode-map "\C-m" 'newline-and-indent)
  (define-key c-mode-map "\C-ce" 'c-comment-edit)
  (setq c-tab-always-indent t)
  (c-set-style "TRPZ")
  (setq tab-width 8)
  (setq c-indent-level 4)
  (setq c-basic-offset 4)
  (my-c-common-func))

;; Objective-C mode
(defun my-objc-mode-hook ()
  ;; (define-key c-mode-map "\C-m" 'newline-and-indent)
  (define-key c-mode-map "\C-ce" 'c-comment-edit)
  (setq c-tab-always-indent t)
  (setq tab-width 8)
  (setq c-indent-level 4)
  (setq c-basic-offset 4)
  (my-c-common-func))
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

;; Perl mode
(defun my-perl-mode-hook ()
  (setq tab-width 8)
  ;; (define-key perl-mode-map "\C-m" 'newline-and-indent)
  (setq perl-indent-level 4)
  (setq perl-continued-statement-offset 4)
  (my-prog-common-hook)
  )

;; TCL mode
(defun my-tcl-mode-hook ()
  (setq tab-width 8)
  ;; (define-key tcl-mode-map "\C-m" 'newline-and-indent)
  (setq tcl-indent-level 4)
  (setq tcl-continued-indent-level 8)
  (setq tcl-tab-always-indent t)
  (setq tcl-auto-newline nil)
  (setq indent-tabs-mode nil)
  (my-prog-common-hook)
  )

;; JavaScript mode
(defun my-js-mode-hook ()
  (setq c-default-style "bsd")
  (setq c-basic-offset 2)
  (setq tab-width 2)
  (setq js-indent-level 2)
  (setq js-continued-statement-offset 2)
  (my-prog-common-hook)
  )

;; Scheme mode...
(defun my-scheme-mode-hook ()
  ;; (define-key scheme-mode-map "\C-m" 'newline-and-indent)
  (my-prog-common-hook)
  )

;; Emacs-Lisp mode...
(defun my-lisp-mode-hook ()
  ;; (define-key lisp-mode-map "\C-m" 'newline-and-indent)
  (define-key lisp-mode-map "\C-i" 'lisp-indent-line)
  (define-key lisp-mode-map "\C-j" 'eval-print-last-sexp)
  (my-prog-common-hook)
  )

;; Text mode
(defun my-text-mode-hook ()
  (setq tab-width 8)
  (setq indent-tabs-mode nil)
  ;; (auto-fill-mode)
  ;; (flyspell-mode)
  )

;; Add all of the hooks...
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'objc-mode-hook 'my-objc-mode-hook)
(add-hook 'scheme-mode-hook 'my-scheme-mode-hook)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'lisp-mode-hook 'my-lisp-mode-hook)
(add-hook 'perl-mode-hook 'my-perl-mode-hook)
(add-hook 'js-mode-hook 'my-js-mode-hook)
(add-hook 'java-mode-hook 'my-js-mode-hook)
(add-hook 'tcl-mode-hook 'my-tcl-mode-hook)
(add-hook 'text-mode-hook 'my-text-mode-hook)

