;;; mmm --- Configuration for mmm-mode  -*- lexical-binding: t -*-
;;; Commentary:
;; mmm-mode by itself is inert, and needs to be configured with rules
;; to tell it where and when to activate sub-modes, and which sub-mode to activate.
;;
;; For example, within JavaScript files, content between "gql`" and "`" should be in
;; `graphql-mode'
;;
;;; Code:
(require 'mmm-mode)
(setf mmm-global-mode 'maybe)

(defun mmm-markdown-autoclass (lang &optional submode)
  "Generate automatic mmm-classes for markdown code blocks.
With the embedded code block language indicated by `LANG'
Optionally specify the sub-mode to use with `SUBMODE', otherwise defaults to
the mode with the name starting with `LANG' and ending with \='mode'"
  (let ((class (intern (concat "markdown-" lang)))
        (submode (or submode (intern (concat lang "-mode"))))
        (front (concat "^(`|~){3,5}" lang "[\r\n]+"))
        (back  (concat "^(`|~){3,5}")))
    (mmm-add-classes (list (list class :submode submode :front front :back back)))
    (mmm-add-mode-ext-class 'markdown-mode nil class)))

(mapc 'mmm-markdown-autoclass '("c" "cpp" "lisp" "rust" "go"))
(mmm-add-classes (list
                  (mmm-markdown-autoclass "js" 'javascript-mode)
                  (mmm-markdown-autoclass "ts" 'tide-mode)))


;; Create a class for embedded GraphQL in JavaScript
(defvar js-graphql-class '(js-graphql :submode graphql-mode :front "gql`" :back "`"))
(mmm-add-classes (list js-graphql-class))
(mmm-add-mode-ext-class 'javascript-mode nil 'js-graphql)

(provide 'mmm)
;;; mmm.el ends here
