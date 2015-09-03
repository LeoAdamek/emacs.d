(require 'package)
(defvar package-list
  ;; These packages will be required and installed automatically.
  ;; If you've forked this, go ahead and edit this as you want
  '(
    php-mode
    nxml
    helm
    yasnippet
    helm-gist
    auto-complete
    rainbow-delimiters
    magit
    origami
    projectile
    helm-projectile
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
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (when (package-installed-p 'php-mode)
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)))


(when (package-installed-p 'go-mode)
  ; Run gofmt on save
  (add-hook 'before-save-hook 'gofmt-before-save)

  ;; Set some keys local to go-mode
  (add-hook 'go-mode-hook (lambda ()
                            (local-key-set (kbd "C-c C-r") 'go-removed-unused-imports)))

  (add-hook 'go-mode-hook (lambda ()
                            (local-key-ket (kbd "C-c i") 'go-goto-imports)))

  ;; Re-bind M-. from `find-tag' to `godef-jump'
  (add-hook 'go-mode-hook (lambda ()
                            (local-key-set (kbd "M-.") 'godef-jump))))

;; Company Mode
(when (package-installed-p 'company)
  (require 'company)
  (require 'company-go)
  (setq company-idle-delay 0)
  (add-hook 'after-init-hook 'global-company-mode))


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
  
;; A little notification goes a long way!
(message "Loaded Packages & Settings")

(provide 'packages)
;;; packages.el ends here
