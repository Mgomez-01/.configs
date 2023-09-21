;;; Compiled snippets and support files for `latex-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'latex-mode
		     '(("problems" "\\begin{document}\n\\maketitle\n\\noindent\n`(let ((counter (string-to-number (read-string \"Enter the range (1-n): \"))))\n   (cl-loop for i from 1 to counter concat\n        (format \"\\\\section{}\\n%d\\n\\\\vspace{2em}\\n\\\\hrule\\n\" i)))`\n\\end{document}\n" "problems" nil nil nil "/home/speedy/.emacs.d/snippets/latex-mode/problems.yasnippet" nil nil)
		       ("inter" "\\intertext{$1}\n  &= $0" "inter" nil nil nil "/home/speedy/.emacs.d/snippets/latex-mode/inter.snippet" nil nil)
		       ("eqns" "\\begin{eqnsection}{$1}{$2}\n    $0\n\\end{eqnsection}" "eqns" nil nil nil "/home/speedy/.emacs.d/snippets/latex-mode/eqns.snippet" nil nil)
		       ("enum" "\\begin{enumerate}\n`(let ((counter (string-to-number (read-string \"Enter the range (1-n): \"))))\n   (cl-loop for i from 1 to counter concat\n              (if (< i counter)\n   	               (format \"\\\\item\\n\")\n            	       (format \"\\\\item\"))))`\n\\end{enumerate}" "enum" nil nil nil "/home/speedy/.emacs.d/snippets/latex-mode/enum.yasnippet" nil nil)))


;;; Do not edit! File generated at Thu Sep 21 00:18:30 2023
