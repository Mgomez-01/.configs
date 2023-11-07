;; Define the list of keywords for Singular.
(defvar singular-keywords
  '("ring" "ideal" "int" "poly" "minpoly" "print")
  "Keywords for Singular.")

;; Create the regexp string for highlighting keywords.
(defvar singular-keywords-regexp (regexp-opt singular-keywords 'words))

;; Define the list of functions for Singular.
(defvar singular-functions
  '("groebner" "std" "size" "factorize" "minbase")
  "Functions for Singular.")

;; Create the regexp string for highlighting functions.
(defvar singular-functions-regexp (regexp-opt singular-functions 'words))

;; Define the rules for highlighting comments.
(defvar singular-comment-regexp "//.*$")

;; Combine the above into a list of highlighting rules.
(defvar singular-font-lock-keywords
  `(
    (,singular-keywords-regexp . font-lock-builtin-face)
    (,singular-functions-regexp . font-lock-function-name-face)
    (,singular-comment-regexp . font-lock-comment-face)
    )
  "Highlighting expressions for Singular mode.")

;; Define the Singular major mode.
(define-derived-mode singular-mode fundamental-mode "Singular"
  "Major mode for editing Singular files."
  ;; Code for syntax highlighting.
  (setq font-lock-defaults '((singular-font-lock-keywords))))

;; Associate Singular mode with ".sing" files.
(add-to-list 'auto-mode-alist '("\\.sing\\'" . singular-mode))

(provide 'singular-mode)
