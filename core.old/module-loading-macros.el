
;;; Require improved (depends on +benchmark-progn):

(require 'lib-benchmark-improved)
(require 'lib-logging-improved)
(require 'lib-defvar-improved)

(defun +load (file &optional noerror nomessage nosuffix must-suffix)
  "Run `load' with soft-error handling and optional benchmarking.
Benchmarking is ran with `+benchmark-action'."
  (condition-case-unless-debug e
      (let ((body (lambda () (load file noerror nomessage nosuffix must-suffix))))
	(if neko-benchmark
	    (+benchmark-action file 'load
	      (funcall body))
	  (progn
	    (funcall body))))
    (error (+warn-error '+load file e))))

(defun +require (feature &optional filename noerror)
  "Run `require' with soft-error handling and optional benchmarking.
Benchmarking is ran with `+benchmark-action'."
  (condition-case-unless-debug e
      (let ((body (lambda () (require feature filename noerror))))
	(if neko-benchmark
	    (+benchmark-action feature 'require
	      (funcall body))
	  (progn
	    (funcall body))))
    (error (+warn-error '+require feature e))))

(eval-after-load 'font-lock
  (font-lock-add-keywords
   'emacs-lisp-mode
   `((,(concat "(\\(\\+require\\)\\_>"   ; match "(+require" and no more
	       "[ \t']*"		       ; match whitespace(s)/quote
	       "\\(\\(?:\\sw\\|\\s_\\)+\\)?") ; match full symbol
      (1 font-lock-keyword-face)
      (2 font-lock-constant-face nil t)))))

(provide 'module-loading-macros)
