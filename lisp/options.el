;;; options -- Sets various Emacs related options
;;
;;; Commentary:
;;
;; File: `options.el`
;;
;; Sets various options for Emacs
;;
;; This file is licensed under GPLv2.
;;
;;; Code:

(defface quote '() "Quote Face." :group 'basic-faces)

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


(defun *configure-frame (frame)
  "Update FRAME settings to my preferences."
  (with-selected-frame frame
    (set-fringe-mode 48)))


(add-hook 'after-make-frame-functions '*configure-frame t)

(defvar my-frame-options-alist '((font . "Iosevka Nerd Font")))
(setq default-frame-alist (append my-frame-options-alist default-frame-alist))
(setq initial-frame-alist (append my-frame-options-alist initial-frame-alist))

;; Add a keybinding to toggle menu-mode.
(global-set-key (kbd "C-c m m") 'toggle-menu-bar-mode-from-frame)

;; Presentation Mode
;; (Once I've built it!)
;; (global-set-key (kbd "C-c m p") 'toggle-presentation-mode)


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
(setq boomark-default-file (expand-file-name "bookmarks" user-emacs-directory))

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

;; Change how duplicate filenames are handled.
;; Change it to filename|parent-directory-until-unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; Load HELM
(when (package-installed-p 'helm)
    (helm-mode t))

;; Load YASnippet
(when (package-installed-p 'yasnippet)
  (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  (global-set-key (kbd "C-c TAB") 'yas-insert-snippet)
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
(add-to-list 'auto-mode-alist '("\\Bowerfile\\'" . ruby-mode))

;; Default is for *.html.php to use html-mode, FAIL!
(add-to-list 'auto-mode-alist '("\\.html\\.php\\'" . php-mode))

;; For .php files within a tmpl folder, set them to web-mode
(add-to-list 'auto-mode-alist '("\\tmpl/\\.php\\'" . web-mode))

;; Set the mode line format
;; mode line
(setq-default mode-line-format
               '(" "
                ;; mode string
                (:propertize global-mode-string face 'mode-line-mode-string)

                ;; file path
                (:propertize (:eval (if (> (length default-directory) 20)
                                        (concat "..." (substring default-directory -23))
                                      default-directory))
                             face 'mode-line-emphasis)

                ;; file name
                (:propertize mode-line-buffer-identification face 'mode-line-emphasis)

                ;; Show if the buffer is modified
                (:propertize mode-line-modified)

                ;; current major mode
                (:propertize "%m" face 'mode-line-emphasis)

                ;; Minor modes
                " +( " minor-mode-alist ")"

                ;; current position in buffer
                " @ line %l, %p"))


(global-flycheck-mode t)

;; Options for C mode
(defvar c-basic-offset 4)

(setq gnus-select-method
      '(nnimap "megumi.breakerofthings.email"))

(provide 'options)

;;; options.el ends here
