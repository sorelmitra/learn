(defun sgml-delete-tagged-text ()
  "delete text between the tags that contain the current point"
  (interactive)
  (let ((b (point)))
    (sgml-skip-tag-backward 1)
    (when (not (eq b (point)))
      ;; moved somewhere, should be at front of a tag now
      (save-excursion
        (search-forward-regexp ">" nil t)
	;; (forward-sexp 1)
        (setq b (point)))
      (sgml-skip-tag-forward 1)
      (search-backward-regexp "<" nil t)
      ;; (backward-sexp 1)
      (delete-region b (point)))))

(defun sgml-delete-surrounding-tags ()
  "delete the tags that surround the current point"
  (interactive)
  (let ((b (point)))
    (sgml-skip-tag-backward 1)
    (when (not (eq b (point)))
      ;; moved somewhere, should be at front of a tag now
      (setq b (point))
      (search-forward-regexp ">" nil t)
      (delete-region b (point))
      (sgml-skip-tag-forward 1)
      (setq b (point))
      (search-backward-regexp "<" nil t)
      (delete-region b (point)))))

(provide 'sgml-sorel)
