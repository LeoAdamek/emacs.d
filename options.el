;;
;; File: options.el`
;;
;; Sets various options for emacs
;;
;; This file is licensed under GPLv2
;;

;; Turn off GUI elements
;; Pretty self explainitory.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Disable the default splash screen
(setq inhibit-splash-screen t)

;; Put all backups in the same place.
;; Avoid polluting repositories.
;; The fact that this isn#t default behaviour is sinful.
;; CURSE YOU STALLMAN!
;; Code from emacswiki/BackupsDirectory
(setq
 backup-by-copying t ; don't clobber symlinks.
 backup-directory-alist '((".*" . ,temporary-file-directory)) ; stop littering
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 auto-save-file-name-transforms '((".*" ,temporary-file-directory t))
 )


 
