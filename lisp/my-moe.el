;;; my-moe --- Custom moe-dark theme
;;; Commentary:
;;;
;;; A Customized theme based on moe-dark
;;; Darker background
;;; Font setting.

;;; Code:

;; My Moe

(let ((moe-theme-dir (expand-file-name "packages/moe-theme" user-emacs-directory)))
  (add-to-list 'load-path moe-theme-dir)
  (require 'moe-theme)
  
  ;; Start with moe-dark
  (moe-dark)
  ;(setq moe-theme-resize-markdown-title '(2.0 1.7 1.5 1.3 1.0 1.0))

  (set-face-attribute 'default nil :background "#111")
  (set-face-attribute 'fringe nil :background "#111")

  ;; Invert the mode line colors
  (let ((mode-line-bg (face-attribute 'mode-line :background))
        (mode-line-fg (face-attribute 'mode-line :foreground)))
    (set-face-attribute 'mode-line nil :background mode-line-fg :foreground mode-line-bg)
    (set-face-attribute 'mode-line-buffer-id nil :background mode-line-fg :foreground mode-line-bg)))

(provide 'my-moe)
