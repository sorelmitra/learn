;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Easy bookmarks management
;;
;; Author: Anthony Fairchild
;;

;;;; Keymaping examples
;; (global-set-key [(control f2)]  'af-bookmark-toggle )
;; (global-set-key [f2]  'af-bookmark-cycle-forward )
;; (global-set-key [(shift f2)]  'af-bookmark-cycle-reverse )
;; (global-set-key [(control shift f2)]  'af-bookmark-clear-all )

;; Include common lisp stuff
(require 'cl)
(require 'bookmark)
(defvar af-current-bookmark nil)

(defun af-bookmark-make-name ()
  "makes a bookmark name from the buffer name and cursor position"
  (concat (buffer-name (current-buffer))
          ":" (number-to-string (point))))

(defun af-bookmark-toggle ()
  "remove a bookmark if it exists, create one if it doesnt exist"
  (interactive)
  (let ((bm-name (af-bookmark-make-name)))
    (if (bookmark-get-bookmark bm-name t)
        (progn (bookmark-delete bm-name)
               (message "bookmark removed"))
      (progn (bookmark-set bm-name)
             (setf af-current-bookmark bm-name)
             (message "bookmark set")))))

(defun af-bookmark-cycle (i)
  "Cycle through bookmarks by i.  'i' should be 1 or -1"
  (if bookmark-alist
      (progn (unless af-current-bookmark
               (setf af-current-bookmark (first (first bookmark-alist))))
             (let ((cur-bm (assoc af-current-bookmark bookmark-alist)))
               (setf af-current-bookmark
                     (if cur-bm
                         (first (nth (mod (+ i (position cur-bm bookmark-alist))
                                          (length bookmark-alist))
                                     bookmark-alist))
                       (first (first bookmark-alist))))
               (bookmark-jump af-current-bookmark)
               ;; Update the position and name of the bookmark.  We
               ;; only need to do this when the bookmark has changed
               ;; position, but lets go ahead and do it all the time
               ;; anyway.
               (bookmark-set-position af-current-bookmark (point))
               (let ((new-name (af-bookmark-make-name)))
                 (bookmark-set-name af-current-bookmark new-name)
                 (setf af-current-bookmark new-name))))
    (message "There are no bookmarks set!")))

(defun af-bookmark-cycle-forward ()
  "find the next bookmark in the bookmark-alist"
  (interactive)
  (af-bookmark-cycle 1))

(defun af-bookmark-cycle-reverse ()
  "find the next bookmark in the bookmark-alist"
  (interactive)
  (af-bookmark-cycle -1))

(defun af-bookmark-clear-all()
  "clears all bookmarks"
  (interactive)
  (setf bookmark-alist nil))

(provide 'bookmarks-anthonyf)
