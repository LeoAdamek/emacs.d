;;
;; LeoAdamek's `emacs.d`
;;
;; File: init.el
;; 
;; Main initialization file
;;
;; This file is licensed under GPLv2
;;

;; Set which files are needed to be loaded.
;; Put the file where "customize-*" options are put.
(setq custom-file
      (expand-file-name "~/.emacs.d/custom.el"))

(load custom-file)
(load "~/.emacs.d/packages.el")
(load "~/.emacs.d/options.el")
(load "~/.emacs.d/functions.el")

;;
;; Hook into a change in mode and load the appropreate file if the correct 
;; after-change-major-mode-hook
;; TBD
(put 'downcase-region 'disabled nil)


(provide 'init)

;;; init.el ends here
