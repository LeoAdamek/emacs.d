;;
;; File: `options.el`
;;
;; Sets various options for emacs
;;
;; This file is licensed under GPLv2
;;
;;; Code:

;; Set some colours & fonts
;; Black background, black fringe, grey text
(custom-set-faces
 '(default ((t (:family "CodingFontTobi" :height 120) )) ))


;; Turn off GUI elements
;; Pretty self explainitory.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Disable the default splash screen
;; Why does it even exist?
(setq inhibit-splash-screen t)

;; Set the *scratch* buffer contents to ""
(setq initial-scratch-message "")

;; Minimal Cursor (looks like a _ )
(setq-default cursor-type 'hbar)

;; Add a fringe.
;; It's quite nice and makes things easier to read
(set-fringe-mode
 (/ (- (frame-pixel-width)
       (* 200 (frame-char-width)))
    16))


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
(setq make-backup-files nil)

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

;; Default theme is `spacegray', from package `spacegray-theme'
;; If that's not installed, load `wombat'
(if (package-installed-p 'spacegray-theme)
    (load-theme 'spacegray)
(load-theme 'wombat))

;; Enable Semantic Mode
(semantic-mode t)

;; Change how duplicate filenames are handled.
;; Change it to filename|parent-directory-until-unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)


;; Load HELM
(when (package-installed-p 'helm)
    (helm-mode t))

;; Load YASnippet
(when (package-installed-p 'yasnippet)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode t))


;; Load Keyfreq if its installed
(when (package-installed-p 'keyfreq)
    (keyfreq-mode t))

;; Set up some useful `auto-mode-alist' mappings

;; Ruby files which don't end in .rb (Gemfile, Guardfile, config.ru)
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("config.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake\\'"   . ruby-mode))
(add-to-list 'auto-mode-alist '("\\Rakefile\\'". ruby-mode))

;; Default is for *.html.php to use html-mode, FAIL!
(add-to-list 'auto-mode-alist '("\\.html\\.php\\'" . php-mode))


;; Set up auto-complete
(when (package-installed-p 'auto-complete)
    (global-auto-complete-mode t))

;; Set the mode line format
;; mode line
(setq-default mode-line-format
              (quote
               (" "
                ;; mode string
                (:propertize global-mode-string face 'mode-line-mode-string)

                ;; file path
                (:propertize (:eval (if (> (length default-directory) 17)
                                        (concat "..." (substring default-directory -20))
                                      default-directory))
                             face 'mode-line-folder-face)

                ;; file name
                (:propertize mode-line-buffer-identification face 'mode-line-buffer-name)
                (:propertize mode-line-modified face 'mode-line-modified-face)
                "  "
                ;; value of 'mode-name'
                (:propertize "%m" face 'mode-line-mode-name)
                " :: "
                ;; line #
                "line %l, %p")))


(global-flycheck-mode t)

;; Options for C mode
(setq c-default-style "k&r"
      c-basic-offset 4)

;; If magit is installed, then set up a new short cut for it
(when (package-installed-p 'magit)
  (global-set-key (kbd "C-c g") 'magit-status))


;; If origami is installed, set it up
(when (package-installed-p 'origami)
  (require 'origami)

  ;; Set the key bindings
  (global-set-key (kbd "C-c f f") 'origami-toggle-node)
  (global-set-key (kbd "C-c f F") 'origami-recursively-toggle-node)
  (global-set-key (kbd "C-c f a") 'origami-toggle-all-nodes)

  ;; Set some modes
  (add-to-list 'origami-parser-alist '(go-mode . origami-c-style-parser))
  (add-to-list 'origami-parser-alist '(php-mode . origami-c-style-parser)))


(provide 'options)

;;; options.el ends here
