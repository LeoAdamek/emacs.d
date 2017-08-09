;;
;; File: `options.el`
;;
;; Sets various options for emacs
;;
;; This file is licensed under GPLv2
;;
;;; Code:

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
       (* 75 (frame-char-width)))
    16))

(set-default-font "Fira Code")



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

;; Option to turn it on *everywhere*
(define-globalized-minor-mode global-hidden-mode-line-mode hidden-mode-line-mode hidden-mode-line-mode-on)

(defun hidden-mode-line-mode-on ()
  (hidden-mode-line-mode 1))


;; And turn it on!
;; (hidden-mode-line-mode t)

(semantic-mode)

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
                (:propertize mode-line-modified)
                "  "
                ;; current major mode
                (:propertize "%m" face 'mode-line-emphasis)

                ;; list of minor modes
                " +" minor-mode-alist

                ;; current position in buffer
                " @ line %l, %p"))


(global-flycheck-mode t)

;; Options for C mode
(defvar c-basic-offset 4)

;; Enable YARD mode if installed, for ruby mode.
(when (package-installed-p 'yard-mode)
  (add-hook 'ruby-mode-hook 'yard-mode))

;; If magit is installed, then set up a new short cut for it
(when (package-installed-p 'magit)
  (global-set-key (kbd "C-c g") 'magit-status)
  (setq magit-last-seen-setup-instructions "1.4.0"))


;; If origami is installed, set it up
(when (package-installed-p 'origami)
  (require 'origami)

  ;; Set the key bindings
  (global-set-key (kbd "C-c f f") 'origami-toggle-node)
  (global-set-key (kbd "C-c f F") 'origami-recursively-toggle-node)
  (global-set-key (kbd "C-c f a") 'origami-toggle-all-nodes)

  ;; Set some modes
  (add-to-list 'origami-parser-alist '(go-mode . origami-c-style-parser))
  (add-to-list 'origami-parser-alist '(php-mode . origami-c-style-parser))

  ;; Hook it up to my modes
  (add-hook 'prog-mode-hook
            (lambda () (origami-mode))))


;; Projectile
(when (package-installed-p 'projectile)
  (require 'projectile)
  (projectile-global-mode)

  (when (package-installed-p 'helm-projectile)
    (require 'helm-projectile)
    (setq projectile-completion-system 'helm)))

;; Flyspell
(when (package-installed-p 'flyspell)
  (require 'flyspell)
  ;; Enable flyspell on modes where it is most useful (but not all)
  (add-hook 'text-mode-hook 'flyspell-mode))


(provide 'options)

;;; options.el ends here
