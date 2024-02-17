;;; Compiled snippets and support files for `tex-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'tex-mode
		     '(("ztx" "  x(n)\\rightarrow{}X(z)\\\\\n  X(z) &= $0" "ztx" nil nil nil "/home/speedy/.emacs.d/snippets/tex-mode/ztx.snippet" nil nil)
		       ("problems" "\\begin{document}\n\\maketitle\n\\noindent\n`(let ((counter (string-to-number (read-string \"Enter the range (1-n): \"))))\n   (cl-loop for i from 1 to counter concat\n        (format \"\\\\section{}\\n%d\\n\\\\vspace{2em}\\n\\\\hrule\\n\" i)))`\n\\end{document}\n" "problems" nil nil nil "/home/speedy/.emacs.d/snippets/tex-mode/problems.yasnippet" nil nil)
		       ("inter" "\\intertext{$1}\n  &= $0" "inter" nil nil nil "/home/speedy/.emacs.d/snippets/tex-mode/inter.snippet" nil nil)
		       ("eqns" "\\begin{eqnsection}{$1}{$2}\n    $0\n\\end{eqnsection}" "eqns" nil nil nil "/home/speedy/.emacs.d/snippets/tex-mode/eqns.snippet" nil nil)
		       ("enum" "\\begin{enumerate}\n`(let ((counter (string-to-number (read-string \"Enter the range (1-n): \"))))\n   (cl-loop for i from 1 to counter concat\n              (if (< i counter)\n   	               (format \"\\\\item\\n\")\n            	       (format \"\\\\item\"))))`\n\\end{enumerate}" "enum" nil nil nil "/home/speedy/.emacs.d/snippets/tex-mode/enum.yasnippet" nil nil)))


;;; Do not edit! File generated at Tue Sep 26 17:24:04 2023
