;; functions --- Utility Functions for (my) emacs
;;; Commentary:
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


;; I need this, dont kill me.
(defun bf-pretty-print-xml-region (begin end)
  "Pretty format XML markup in region. You need to have nxml-mode
http://www.emacswiki.org/cgi-bin/wiki/NxmlMode installed to do
this.  The function inserts linebreaks to separate tags that have
nothing but whitespace between them.  It then indents the markup
by using nxml's indentation rules."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t) 
        (backward-char) (insert "\n"))
      (indent-region begin end))
    (message "Region formatted."))


(provide 'functions)

;;; functions.el ends here
