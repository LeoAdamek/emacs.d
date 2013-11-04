;;
;; File: options.el`
;;
;; Sets various options for emacs
;;
;; This file is licensed under GPLv2
;;

;; Turn off GUI elements
;; Pretty self explainitory.
(menu-bar-mode 1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Disable the default splash screen
(setq inhibit-splash-screen t)

;; Turn ON ido-mode
;; NOTE: I've disabled this to try and use HELM
;;(ido-mode 1)

;; Put all backups in the same place.
;; Avoid polluting repositories.
;; The fact that this isn#t default behaviour is sinful.
;; CURSE YOU STALLMAN!
;; Code from emacswiki/BackupsDirectory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Put the bookmarks in ~/.emacs.d/bookmarks
(setq boomark-default-file "~/.emacs.d/bookmarks")

;; Answer Yes/No prompts with Y/N instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Use Paren-mode -- It highlights matching parenthesies
;;                   When the cursor is over one
(require 'paren)
(show-paren-mode t)

;; Truncate long lines. equivelent to `nowrap` in Vi
(setq-default truncate-lines t)

;; Use soft-tabs by default (spaces inserted when tab pressed)
(setq-default indent-tabs-mode nil)

;;
;; If ECB is installed, bind C-e C-e to open it.
;;
(eval-after-load "ecb.el" '(global-set-key [(control ?e) (?p)]))

;; Edit PHP with `web-mode'
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

;; Try to load `color-theme-sanityinc-tomorrow'
;; If that's not installed, load `wombat'
(if (package-installed-p 'color-theme-sanityinc-tomorrow)
    (color-theme-sanityinc-tomorrow-night)
  (load-theme 'wombat))

;; Enable Semantic Mode
(semantic-mode t)

;; Change how duplicate filenames are handled.
;; Change it to filename|parent-directory-until-unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)


(if (package-installed-p 'helm)
    (helm-mode t))

(if (package-installed-p 'yasnippet)
    (yas-global-mode t))


(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))


        
