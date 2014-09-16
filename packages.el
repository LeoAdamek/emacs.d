(require 'package)
(defvar package-list
  '(
    php-mode
    nxml
    helm
    yasnippet
;;    moe-theme
    helm-gist
    auto-complete
    rainbow-delimiters
    magit
    )
)

;; Add Package Archives
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(if (file-exists-p "./packages/slime/slime.el")
    (progn
      (add-to-list 'load-path "./packages/slime")
      (require 'slime-autoloads)

      (setq inferior-lisp-program "sbcl")

      (message "Loaded SLIME for SBCL"))
  (message "Install SLIME and SBCL for awesome LISPing"))

(provide 'packages)
;;; packages.el ends here
