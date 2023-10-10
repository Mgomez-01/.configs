;;; cnf-mode.el --- Major mode for editing CNF files -*- lexical-binding: t; -*-

;; Author: Your Name
;; URL: https://github.com/yourgithub/cnf-mode
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; This package provides a major mode for editing files in Conjunctive Normal Form (CNF).
;; Features include syntax highlighting, ...

;;; Code:
(defface cnf-negative-literal-face
  '((t :foreground "red"))
  "Face for highlighting negative literals in CNF mode.")


(defvar cnf-mode-syntax-table nil "Syntax table for `cnf-mode'.")

(setq cnf-mode-syntax-table
      (let ((syn-table (make-syntax-table)))
        (modify-syntax-entry ?c "< b" syn-table)
        (modify-syntax-entry ?\n "> b" syn-table)
        syn-table))

(defvar cnf-font-lock-keywords
  (list
   '("^p cnf" . font-lock-keyword-face)
   '("^c.*" . font-lock-comment-face)
   '("-[0-9]+" . 'cnf-negative-literal-face)  ; custom face for negative literals
   '("[0-9]+" . font-lock-constant-face))
  "Keyword highlighting specification for `cnf-mode'.")


(message "Loading cnf-mode")
(define-derived-mode cnf-mode fundamental-mode "CNF"
  "A major mode for editing Conjunctive Normal Form (CNF) files."
  :syntax-table cnf-mode-syntax-table
  (setq-local font-lock-defaults '(cnf-font-lock-keywords))
  (setq-local comment-start "c")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+\\s-*"))
(add-to-list 'auto-mode-alist '("\\.cnf\\'" . cnf-mode))
(message "provide cnf-mode")
(provide 'cnf-mode)

;;; cnf-mode.el ends here
