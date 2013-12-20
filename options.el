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
;; The fact that this isn't default behaviour is sinful.
;; CURSE YOU STALLMAN!
;; Code from emacswiki/BackupsDirectory
(defconst emacs-tmp-dir (format "%s/emacs/%s" temporary-file-directory (user-uid)))

(setq backup-directory-alist
      `((".*" . ,emacs-tmp-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-tmp-dir t)))
(setq auto-save-list-file-prefix emacs-tmp-dir)

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

;; Edit PHP with `web-mode'
;; `web-mode' isn't really that great :(
;; (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))

;; Try to load `color-theme-sanityinc-tomorrow'
;; If that's not installed, load `wombat' - defaultest dark theme.
(if (package-installed-p 'color-theme-sanityinc-tomorrow)
    (load-theme 'sanityinc-tomorrow-bright)
(load-theme 'wombat))
;;(load-theme 'hemisu-dark)


;; Enable Semantic Mode
(semantic-mode t)

;; Change how duplicate filenames are handled.
;; Change it to filename|parent-directory-until-unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)


;; Load HELM
(if (package-installed-p 'helm)
    (helm-mode t))

;; Load YASnippet
(if (package-installed-p 'yasnippet)
    (yas-global-mode t))

;; Load some Snippets
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
