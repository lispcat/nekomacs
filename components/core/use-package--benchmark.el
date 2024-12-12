

(require 'benchmark)
(require 'use-package-core) ;; does this suffice?

;;; variable to enable keyword by default:

;; (defcustom use-package-benchmark-by-default t
;;   "Non-nil enables benchmarking on all use-package invocations."
;;   :type 'boolean
;;   :group 'use-package)

;;; add use-package ":benchmark" keyword:

(defalias 'use-package-normalize/:benchmark 'use-package-normalize-predicate)

(defun use-package-handler/:benchmark (name _keyword arg rest state)
  "Wrap the `use-package` declaration in `benchmark-progn` when `:benchmark t` is used."
  (let ((body (use-package-process-keywords name rest state)))
    (if arg
        `((+benchmark-action ',name 'use-package
	    ,@body))
      body)))

(add-to-list 'use-package-keywords :benchmark)

;; add ":benchmark" to the default list of use-package keywords
(setq use-package-defaults
      (cons '(:benchmark
	      '(t)
	      ;; use-package-benchmark-by-default
	      nekomimi-benchmark
	      )
	    use-package-defaults))

(provide 'use-package--benchmark)
