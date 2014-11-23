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
    )
)

;; Add Package Archives (MELPA and Marmalade)
;; Yes, I know MELPA is unpopular but whatever.
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)

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

(provide 'packages)
;;; packages.el ends here
