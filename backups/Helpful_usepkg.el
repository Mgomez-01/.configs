;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.


; example of the call below being expanded with M-x emacs-lisp-macroexpand
  (use-package vertico
    :init
      (vertico-mode t))



(progn
  (defvar use-package--warning12
    #'(lambda
	(keyword err)
	(let
	    ((msg
	      (format "%s/%s: %s" 'vertico keyword
		      (error-message-string err))))
	  (display-warning 'use-package msg :error))))
  (condition-case-unless-debug err
      (progn
	(condition-case-unless-debug err
	    (vertico-mode t)
	  (error
	   (funcall use-package--warning12 :init err)))
	(if
	    (not
	     (require 'vertico nil t))
	    (display-warning 'use-package
			     (format "Cannot load %s" 'vertico)
			     :error)))
    (error
     (funcall use-package--warning12 :catch err))))
