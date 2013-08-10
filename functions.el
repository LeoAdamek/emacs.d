;;
;;
;; Leo Adamek's emacs.d
;;
;;
;; functions.el
;;
;; Useful universal functions, ideal at any time.
;;
;; License: GPLv2
;;


;;==========================================================
;;  Buffer Functions;
;;==========================================================

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun kill-all-dired-buffer ()
  "Kill all dired buffers."
  (interactive)
  (save-excursion
    (let ((count 0))
      (dolist (buffer (buffer-list))
	(set-buffer buffer)
	(when (equal major-mode 'dired-mode)
	  (setq count (1+ count))
	  (kill-buffer buffer)))
      (message "Killed %i dired buffer(s)." count))))


;;
;; Bookmark Functions
;;

(defun bookmark-to-abbrevs ()
  "Create Abbrevs based on the `bookmark-alist'."
  (dolist (bookmark bookmark-alist)
    (lst* ((name (car bookmark))
	   (file (bookmark-get-filename name)))
	  (define-abbrev global-abbrev-table name file))))
