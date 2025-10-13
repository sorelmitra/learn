;; I-search with initial contents


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variant 1: Automatically start isearch (doesn't work very well)

(defvar isearch-initial-string nil)

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (message-box "isearch-initial-string 2 %s" isearch-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
	   (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
	  (isearch-forward regexp-p no-recursive-edit)
        ;; (concat "\\<"  "\\>")
	(setq isearch-initial-string
	      (buffer-substring-no-properties begin end))
        (message-box "isearch-initial-string %s" isearch-initial-string)
	(add-hook 'isearch-mode-hook 'isearch-set-initial-string)
	(isearch-forward regexp-p no-recursive-edit)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variant 2: Start isearch normally, then yank the symbol
;;
;; Usage:
;; - Bind some key to isearch-yank-symbol:
;;   (define-key isearch-mode-map (kbd "C-e") 'isearch-yank-symbol)
;; - To match the symbol not whole-word, start a isearch, then press the key
;; - To match a whole-word symbol, start a regexp isearch, then press the key

(defun isearch-yank-and-run (regexp)
  "Pull REGEXP into search regexp." 
  (let ((isearch-regexp nil)) ;; Dynamic binding of global.
    (isearch-yank-string regexp))
  (isearch-search-and-update))

(defun isearch-yank-symbol ()
  "Put symbol at current point into search string."
  (interactive)
  (let
      ((end (progn (skip-syntax-forward "w_") (point)))
       (begin (progn (skip-syntax-backward "w_") (point)))
       )
    (setq sym (buffer-substring begin end))
    (if (null sym)
	(message "No symbol at point")
      (if (not isearch-regexp)
	  (isearch-yank-and-run (concat "" sym))
	(isearch-yank-and-run (concat "\\<" (regexp-quote sym) "\\>"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variant 3: Works, the only drawback is that it doesn't automatically
;; start search, it just fills the word under cursor

(defun my-isearch-word-at-point ()
  (interactive)
  (call-interactively 'isearch-forward-regexp))

(defun my-isearch-yank-word-hook ()
  (when (equal this-command 'my-isearch-word-at-point)
    (let ((string (concat "\\<"
                          (buffer-substring-no-properties
                           (progn (skip-syntax-backward "w_") (point))
                           (progn (skip-syntax-forward "w_") (point)))
                          "\\>")))
      (if (and isearch-case-fold-search
               (eq 'not-yanks search-upper-case))
          (setq string (downcase string)))
      (setq isearch-string string
            isearch-message
            (concat isearch-message
                    (mapconcat 'isearch-text-char-description
                               string ""))
            isearch-yank-flag t)
      ;; (isearch-search-and-update)
      )))

(add-hook 'isearch-mode-hook 'my-isearch-yank-word-hook)

(provide 'find-word)

