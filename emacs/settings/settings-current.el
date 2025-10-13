(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2e3436" "#a40000" "#4e9a06" "#c4a000" "#204a87" "#5c3566" "#729fcf" "#eeeeec"])
 '(blink-matching-paren nil)
 '(column-number-mode t)
 '(compilation-scroll-output t)
 '(cua-mode t nil (cua-base))
 '(cua-normal-cursor-color "black")
 '(cursor-type (quote (bar . 2)))
 '(custom-enabled-themes nil)
 '(ediff-merge-split-window-function (quote split-window-horizontally))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fill-column 80)
 '(global-font-lock-mode t)
 '(grep-command "eg . ")
 '(grep-find-command (quote ("find . -type f -exec grep -nH -e  {} +" . 34)))
 '(grep-find-template "find . <X> -type f <F> -exec grep <C> -nH -e <R> {} +")
 '(grep-highlight-matches nil)
 '(grep-template "grep <X> <C> -nH -e <R> <F>")
 '(grep-use-null-device nil)
 '(gud-gdb-command-name "gdb --annotate=1")
 '(iswitchb-mode t)
 '(large-file-warning-threshold nil)
 '(safe-local-variable-values
   (quote
    ((js-continued-statement-offset . 2)
     (c-indent-level . 2)
     (c-indent-level . 4))))
 '(savehist-mode t nil (savehist))
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(tags-revert-without-query 1)
 '(tags-table-list
   (quote
    ("/Volumes/Data/w/VOYAGER/DEV/branches/Voyager/upgradepkg/TAGS")))
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify))
 '(uniquify-separator "|")
 '(x-select-enable-clipboard t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Path for various modules
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'package)
;(add-to-list 'package-archives
;             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; no startup screen
(setq inhibit-startup-screen t)

(set-face-attribute 'default nil
		    :family "Monaco" :height 140 :weight 'normal)

(defalias 'yes-or-no-p 'y-or-n-p)

;; "open with" in same frame rather than in new frames (Emacs 23.3+)
(setq ns-pop-up-frames nil)

;; remember cursor positions
(require 'saveplace)
(setq-default save-place t)

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed 't) ;; accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; display selection
(setq transient-mark-mode t)

;; PgUp/PgDn return you to the same line where you left from
(setq scroll-preserve-screen-position t)

;; don't split windows horizontally
(setq split-height-threshold 60) ; split vertically if it has at least n columns
(setq split-width-threshold nil)

;disable backup
(setq backup-inhibited t)

;; reload tags automatically
(setq tags-revert-without-query 1)

;; enable autoindent by default
(global-set-key (kbd "RET") 'newline-and-indent)

;; isearch: Activate occur easily
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(defun kill-current-buffer ()
  "Kill the current buffer, without confirmation."
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "<C-return>") 'cua-rectangle-mark-mode)

(global-set-key (kbd "<f8>") 'find-file)
(global-set-key (kbd "<M-f8>") 'revert-buffer)
(global-set-key (kbd "ESC <f8>") 'revert-buffer)
(global-set-key (kbd "<f2>") 'save-buffer)
(global-set-key (kbd "<S-f2>") 'save-some-buffers)
(global-set-key (kbd "<M-f2>") 'kill-current-buffer)
(global-set-key (kbd "ESC <f2>") 'kill-current-buffer)
(global-set-key (kbd "C-6") 'switch-to-buffer)
(global-set-key (kbd "C-^") 'switch-to-buffer)

(global-set-key (kbd "<f5>") 'next-error)
(global-set-key (kbd "<S-f5>") 'previous-error)
(global-set-key (kbd "<f6>") 'next-multiframe-window)
(global-set-key (kbd "<S-f6>") 'previous-multiframe-window)

(global-set-key (kbd "<f7>") 'compile)
(global-set-key (kbd "<f9>") 'grep)
(global-set-key (kbd "<S-f9>") 'find-dired)

(global-set-key (kbd "<f11>") 'find-tag)
(global-set-key (kbd "<S-f11>") 'find-tag-other-window)
;; (global-set-key (kbd "<S-f11>") 'menu-bar-next-tag)
(global-set-key (kbd "<f12>") 'tags-apropos)
(global-set-key (kbd "<S-f12>") 'find-tag-regexp)

;; Graphviz dot language
(load "graphviz-dot-mode.el")

;; Associate files with modes
(add-to-list 'auto-mode-alist '("\\.pic\\'" . gnuplot-mode))

;; Settings for all languages
(defun my-prog-common-hook ()
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
  (setq indent-tabs-mode nil) ;; force only spaces for indentation
  (setq post-self-insert-hook nil) ; disable all electric characters (Emacs 24+)
  )

(add-hook 'latex-mode-hook 'my-prog-common-hook)
(add-hook 'c++-mode-hook 'my-prog-common-hook)
(add-hook 'c-mode-hook 'my-prog-common-hook)
(add-hook 'objc-mode-hook 'my-prog-common-hook)
(add-hook 'scheme-mode-hook 'my-prog-common-hook)
(add-hook 'emacs-lisp-mode-hook 'my-prog-common-hook)
(add-hook 'lisp-mode-hook 'my-prog-common-hook)
(add-hook 'perl-mode-hook 'my-prog-common-hook)
(add-hook 'js-mode-hook 'my-prog-common-hook)
(add-hook 'java-mode-hook 'my-prog-common-hook)
(add-hook 'tcl-mode-hook 'my-prog-common-hook)
(add-hook 'text-mode-hook 'my-prog-common-hook)
(add-hook 'gnuplot-mode-hook 'my-prog-common-hook)
(add-hook 'graphviz-dot-mode 'my-prog-common-hook)
