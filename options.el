;;
;; File: `options.el`
;;
;; Sets various options for emacs
;;
;; This file is licensed under GPLv2
;;

;; Set some colours & fonts
;; Black background, black fringe, grey text
(custom-set-faces
 '(default ((t (:background "black" :foreground "white" :family "ProggyTiny" :height 80) )) )
 '(fringe  ((t (:background "black") )) ) )

;; Turn off GUI elements
;; Pretty self explainitory.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Disable the default splash screen
(setq inhibit-splash-screen t)

;; Set the *scratch* buffer default to ""
(setq initial-scratch-message "")

;; Minimal Cursor (looks like a _ )
(setq-default cursor-type 'hbar)

;; Add a fringe.
;; It's quite nice and makes things easier to read
(set-fringe-mode
 (/ (- (frame-pixel-width)
       (* 200 (frame-char-width)))
    8))


;; Turn off the mode line Ö :shocking:
(defvar-local hidden-mode-line-mode nil)
(define-minor-mode hidden-mode-line-mode
  "Minor mode to hide the mode-line in the current buffer."
  :init-value nil
  :global t
  :variable hidden-mode-line-mode
  :group 'editing-basics
  (if hidden-mode-line-mode
      (setq hide-mode-line mode-line-format
            mode-line-format nil)
    (setq mode-line-format hide-mode-line
          hide-mode-line nil))
  (when (and (called-interactively-p 'interactive)
             hidden-mode-line-mode)
    (run-with-idle-timer
     0 nil 'message
     (concat "Hidden mode line enabled. "
             "Use M-x hidden-mode-line-mode RET to return it.") )))

;; And turn it on!
(hidden-mode-line-mode t)


;; Have a mode-line in C-s-c in the header
(defun mode-line-in-header ()
  (interactive)
  (if (not header-line-format)
      (setq header-line-format mode-line-format)
    (setq header-line-format nil))
  (force-mode-line-update))
 (global-set-key (kbd "C-s-c") 'mode-line-in-header)

;; Put all backups in the same place.
;; Avoid polluting repositories.
;; The fact that this isn't default behaviour is sinful.
;; CURSE YOU STALLMAN!
;; Code from emacswiki/BackupsDirectory
(defconst emacs-tmp-dir (make-temp-file "emacs-temp" 'dir))

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

;; Default theme is `moe-dark', from package `moe-theme'
;; If that's not installed, load `wombat'
(if (package-installed-p 'moe-theme)
    (load-theme 'moe-dark)
  (load-theme 'wombat))

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

;; Load Keyfreq if its installed
(if (package-installed-p 'keyfreq)
    (keyfreq-mode t))



;; Set up some useful `auto-mode-alist' mappings

;; Ruby files which don't end in .rb (Gemfile, Guardfile, config.ru)
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("config.ru\\'" . ruby-mode))

;; Default is for *.html.php to use html-mode, FAIL!
(add-to-list 'auto-mode-alist '("\\.html\\.php\\'" . php-mode))


;; Set up auto-complete
(if (package-installed-p 'auto-complete)
    (global-auto-complete-mode t))

;; Rainbow delimiters if available
(if (package-installed-p 'rainbow-delimiters)
    (global-rainbow-delimiters-mode t))
