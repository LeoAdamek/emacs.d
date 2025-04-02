;; emacs.d --- LeoAdamek's `emacs.d`
;; Commentary:
;; Main initialization file
;;
;; This file is licensed under GPLv2
;;

;;; Code:
;; Set which files are needed to be loaded.
;; Put the file where "customize-*" options are put
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(message "Loading Packages")
(require 'packages)

(message "Loading Options")
(require 'options)

(message "Loading Functions")
(require 'functions)

;(defvar host-options-file (concat (expand-file-name "lisp/") (system-name) ".el"))
;(when (file-exists-p host-options-file)p
;  (load host-options-file)
;  (message (concat "Loaded options for " (system-name))))

(provide 'init)

;;; init.el ends here

