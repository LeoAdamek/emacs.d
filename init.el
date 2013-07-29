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

(load "~/.emacs.d/options.el")
