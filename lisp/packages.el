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
    rainbow-delimiters

    ;; Magit (Git Integration)
    magit

    ;; Tide (TypeScript)
    tide

    ;; Origami (code folding)
    origami

    ;; Projectile
    projectile
    helm-projectile

    ;; RESTClient interacts with HTTP from Emacs buffers
    restclient
    restclient-helm
    company-restclient
    
    )
)

;; Add Package Archives (MELPA and Marmalade)

;; Yes, I know MELPA is unpopular but whatever.
;; I may remove it in the future. Be warned!
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Marmalade:: The best ELPA host.
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Install all the requested packages
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))


;; Load SLIME if SBCL and SLIME are installed.
;; Slime is /not/ installed via package.el
(when (file-exists-p "~/.emacs.d/packages/slime/slime.el")
      (add-to-list 'load-path "~/.emacs.d/packages/slime")
      (require 'slime-autoloads)
      (setq inferior-lisp-program "sbcl")
      (message "Loaded SLIME for SBCL"))

(when (file-exists-p "~/.emacs.d/packages/moe-theme/moe-theme.el")
  (add-to-list 'load-path "~/.emacs.d/packages/moe-theme")
  (require 'my-moe))

(when (package-installed-p 'haskell-mode)
  (require 'haskell-interactive-mode)
  (require 'haskell-process)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (defvar haskell-font-lock-symbols t))

(when (package-installed-p 'coffee-mode)
  (defvar coffee-tab-width 2))

(when (package-installed-p 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))



(when (package-installed-p 'go-mode)
  (setq gofmt-command "goimports")
  ; Run gofmt on save
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; Set some keys local to go-mode
  ;; Re-bind M-. from `find-tag' to `godef-jump'
  (add-hook 'go-mode-hook (lambda ()
                            (local-key-set (kbd "C-c C-r") 'go-removed-unused-imports)
                            (local-key-set (kbd "C-c i") 'go-goto-imports)
                            (local-key-set (kbd "M-.") 'godef-jump))))





;; Company Mode
(when (package-installed-p 'company)
  (require 'company)
  (setq company-idle-delay 0)
  (setq company-tooltip-align-annotations t)
  (add-hook 'after-init-hook 'global-company-mode))

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

(when (and (package-installed-p 'racer) (package-installed-p 'rust-mode) (package-installed-p 'racer))
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (require 'rust-mode)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))

(when (package-installed-p 'wakatime-mode)
  (global-wakatime-mode))
  
;; A little notification goes a long way!
(message "Loaded Packages & Settings")

(provide 'packages)
;;; packages.el ends here
