;;; my-moe --- Custom moe-dark theme
;;; Commentary:
;;;
;;; A Customized theme based on moe-dark
;;; Darker background
;;; Font setting.

;;; Code:

;; My Moe
(require 'moe-theme)

;; Start with moe-dark
(moe-dark)
(setq moe-theme-resize-markdown-title '(2.0 1.7 1.5 1.3 1.0 1.0))

;; Set some colours & fonts
;; Black background, black fringe, grey text
(custom-set-faces
 '(default ((t (:background "#111" :height 120 :family "CodingFontTobi"))) )
 '(fringe ((t (:background "#111")))))

(provide 'my-moe)
