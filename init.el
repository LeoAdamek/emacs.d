;; emacs.d --- LeoAdamek's `emacs.d`
;; Commentary:
;; Main initialization file
;;
;; This file is licensed under GPLv2
;;

;;; Code:
;; Set which files are needed to be loaded.
;; Put the file where "customize-*" options are put.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq custom-file
      (expand-file-name "~/.emacs.d/custom.el"))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(message "Loading Packages")
(require 'packages)

(message "Loading Options")
(require 'options)

(message "Loading Functions")
(require 'functions)

(defvar host-options-file (concat (expand-file-name "~/.emacs.d/lisp/") (system-name) ".el"))
(when (file-exists-p host-options-file)
  (load host-options-file)
  (message (concat "Loaded options for " (system-name))))

(provide 'init)

;;; init.el ends here
(put 'upcase-region 'disabled nil)
