;; emacs.d --- LeoAdamek's `emacs.d`
;; Commentary:
;; Main initialization file
;;
;; This file is licensed under GPLv2
;;

;;; Code:
;; Set which files are needed to be loaded.
;; Put the file where "customize-*" options are put.
(setq custom-file
      (expand-file-name "~/.emacs.d/custom.el"))

(add-to-list 'load-path "~/.emacs.d")

(message "Loading Custom file")
(load custom-file)

(message "Loading Packages")
(require 'packages)

(message "Loading Options")
(require 'options)

(message "Loading Functions")
(require 'functions)

;;
;; Hook into a change in mode and load the appropreate file if the correct 
;; after-change-major-mode-hook
;; TBD
(put 'downcase-region 'disabled nil)


(provide 'init)

;;; init.el ends here
