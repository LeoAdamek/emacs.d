(require 'package)
(defvar package-list
  '(
    web-mode
    php-mode
    mmm-mode
    nxml
    helm
    yasnippet
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
