(defvar feko-mode-syntax-table nil "Syntax table for `feko-mode'.")
;; Define a new face
(defface feko-uppercase-face
  '((t :foreground "blue" :weight bold))
  "Face for highlighting uppercase words in My Custom Mode")

(setq feko-mode-syntax-table
      (let ((table (make-syntax-table)))
        ;; comment style: "*...*"
        (modify-syntax-entry ?* "< b" table)
        (modify-syntax-entry ?\n "> b" table)
        table))

(defvar feko-mode-font-lock-keywords
  (list
   ;; highlight metadata like "Creator: (null) 2023-10-20, 19:11:39 (-0600)"
   '("Creator: \\(.*\\) [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}, [0-9:\\-\\+]*" 1 font-lock-variable-name-face)
   ;; Highlight numbers
   '("\\b\\([0-9]+\\)\\b" . font-lock-constant-face)
   ;; Highlight all uppercase words, with or without underscores
   '("\\(?:^\\|\\s-\\)\\([A-Z]+\\|[A-Z_]+\\|[0-9A-Z]+\\|[0-9A-Z_]+\\)\\(?:$\\|\\s-\\)" . dired-directory-face)
   ;; Highlight single characters
   '("'\\(\\w\\)'" . font-lock-string-face)  ; for characters in quotes
   )
  "Highlighting for My Custom Mode")

(define-derived-mode feko-mode fundamental-mode "MyCustom"
  "Major mode for editing custom project files."
  :syntax-table feko-mode-syntax-table
  (setq font-lock-defaults '((feko-mode-font-lock-keywords))))

;; Automatically use feko-mode for .mic, .net, .nup, .wpi files
(add-to-list 'auto-mode-alist '("\\.mic\\'" . feko-mode))
(add-to-list 'auto-mode-alist '("\\.net\\'" . feko-mode))
(add-to-list 'auto-mode-alist '("\\.nup\\'" . feko-mode))
(add-to-list 'auto-mode-alist '("\\.wpi\\'" . feko-mode))
(provide 'feko-mode)
