;;; 

;;; Commentary:

;; When running `defvar', if the variable is previously defined with `defvar',
;; its original value is not overwritten. However, this does not apply if
;; previously defined with `setq', or if `defvar' is run interactively.
;;
;; This package provides `+defvar', which unlike the former, never overwrites
;; the value if previously defined in any way.

;;; Code:

(defmacro +defvar (symbol &optional initvalue docstring)
  "The same as `defvar' but INITVALUE is only set if SYMBOL is unset.
So if SYMBOL already exists with a value, it is not changed.
But the DOCSTRING is set no matter the condition."
  (declare (indent defun)
	   (doc-string 3))
  `(progn
     (defvar ,symbol ,(if (boundp symbol)
			  symbol
			initvalue)
       ,docstring)))

(put '+defvar 'lisp-define-type 'var)
(put '+defvar 'edebug-form-spec '(symbolp &optional form stringp))

;; syntax highlighting:

(eval-after-load 'font-lock
  (font-lock-add-keywords
   'emacs-lisp-mode
   `((,(concat "(\\+defvar\\_>"		; match "(+defvar" and no more
	       "[ \t']*"		; match whitespace(s)/quote
	       "\\(\\(?:\\sw\\|\\s_\\)+\\)?") ; match a symbol
      (1 'font-lock-variable-name-face nil t)))))

(provide 'lib-defvar-improved)
