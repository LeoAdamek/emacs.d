;;;; packages -- Sets configuration options for external packages
;;; -*- lexical-binding: t -*-
;;
;;
;;; Commentary:
;;
;; Configures packages sourced from external elisp repositories,
;; defines which packages should always be installed, additional packages
;; can be installed manually...
;;
;;; Code:


(require 'package)
(defvar package-list
  ;; These packages will be required and installed automatically.
  ;; If you've forked this, go ahead and edit this as you want
  '(
    ;; PHP mode.
    php-mode

    ;; XML editing tools
    nxml

    ;; Helm puts you in control
    helm

    ;; YASnippet - Powerful snippet engine
    yasnippet

    ;; Rainbow Delimiters - Colour-codes levels of delimiters
    ;; Unbelivably necessary for LISP.
    rainbow-delimiters

    ;; Magit (Git Integration)
    magit

    ;; Projectile
    projectile

    ;; Code completions
    company

    ;; Origami - Code folding
    origami

    ;; TypeScript development
    tide

    ;; Code checking (on-the-fly)
    flycheck

    ;; LISP Superpowers
    slime
    slime-company
    )
)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Install all the requested packages
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

;; Company Mode
(when (package-installed-p 'company)
  (use-package company
    :config (setq company-idle-delay 0.2
                  company-tooltip-align-annotations t
                  company-minimum-prefix-length 1
                  company-format-margin-function 'company-vscode-dark-icons-margin))
  (add-hook 'after-init-hook 'global-company-mode))

;; Load SLIME if SBCL and SLIME are installed.
(when (package-installed-p 'slime)
  (use-package slime
    :config (setf inferior-lisp-program "sbcl"))
  (when (package-installed-p  'slime-company)
    (slime-setup '(slime-fancy slime-company))))


(let ((moe-dir (expand-file-name "packages/moe-theme" user-emacs-directory)))
  (when (file-exists-p moe-dir)
    (add-to-list 'load-path moe-dir)
    (require 'my-moe)))

(when (package-installed-p 'haskell-mode)
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (defvar haskell-font-lock-symbols t))

(when (package-installed-p 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))


(when (package-installed-p 'tide)
  (add-hook 'typescript-mode-hook (lambda ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier +1)))
  (add-hook 'before-save-hook 'tide-format-before-save))

(when (package-installed-p 'robe)
  (add-hook 'ruby-mode-hook (lambda ()
                              (interactive)
                              (robe-mode +1)
                              (eldoc-mode +1)
                              (setq flycheck-check-syntax-automatically '(save mode-enabled))
                              (flycheck-mode +1))))



;; Stop company and yasnippit fighting
(when (and (package-installed-p 'company) (package-installed-p 'yasnippet))
  "Shamelessly copied from https://gist.github.com/nonsequitur/265010"
  (define-key company-active-map "\t" 'company-yasnippet-or-completion)

  (defun company-yasnippet-or-completion ()
    (interactive)
    (if (yas/expansion-at-point)
        (progn (company-abort)
               (yas/expand))
      (company-complete-common)))

  (defun yas/expansion-at-point ()
    "Tested with v0.6.1. Extracted from `yas/expand-1'"
    (first (yas/current-key))))

(when (package-installed-p 'helm)
  (global-unset-key (kbd "M-x"))
  (global-set-key (kbd "M-x") 'helm-M-x))

(when (and (package-installed-p 'racer) (package-installed-p 'rust-mode) (package-installed-p 'racer))
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (require 'rust-mode)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))

(when (package-installed-p 'wakatime-mode)
  (global-wakatime-mode))



(when (package-installed-p 'go-mode)
  (setq gofmt-command "goimports")
  ; Run gofmt on save
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Set some keys local to go-mode
  ;; Re-bind M-. from `find-tag' to `godef-jump'
  (add-hook 'go-mode-hook (lambda ()
;                            (local-key-set (kbd "C-c C-r") 'go-removed-unused-imports)
;                            (local-key-set (kbd "C-c i") 'go-goto-imports)
;                            (local-key-set (kbd "M-.") 'godef-jump)
                            (when (package-installed-p 'company-go)
                              (message "Setting company backend to company-go")
                              (set (make-local-variable 'company-backends) '(company-go))))))



;; Enable YARD mode if installed, for ruby mode.
(when (package-installed-p 'yard-mode)
  (add-hook 'ruby-mode-hook 'yard-mode))

;; If magit is installed, then set up a new short cut for it
(when (package-installed-p 'magit)
  (global-set-key (kbd "C-c g") 'magit-status)
  (defvar magit-last-seen-setup-instructions "1.4.0"))

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

;; A little notification goes a long way!
(message "Loaded Packages & Settings")

(provide 'packages)
;;; packages.el ends here


