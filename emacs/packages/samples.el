(defun select-current-word ()
"Select the word under cursor.
“word” here is considered any alphanumeric sequence with “_” or “-”."
 (interactive)
 (let (pt)
   (skip-chars-backward "-_A-Za-z0-9")
   (setq pt (point))
   (skip-chars-forward "-_A-Za-z0-9")
   (set-mark pt)
 ))
