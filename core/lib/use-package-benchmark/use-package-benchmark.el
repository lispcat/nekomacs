
;;; Benchmarking improved

(require 'benchmark)
(require 'use-package-core)

;;; variable to enable keyword by default:

(defcustom use-package-benchmark-by-default t
  "Non-nil enables benchmarking on all use-package invocations."
  :type 'boolean
  :group 'use-package)

;; benchmarking macro

(defmacro use-package-benchmark--macro (feature action &rest body)
  "A slight modification to the `benchmark-progn' macro.
It now prints FEATURE when printing the time taken to eval BODY.
ACTION can be either \\='require or \\='load, which prepends FEATURE with either
\"Required: \" or \"Loaded: \". If neither match, print \\=`\",ACTION: \"."
  (declare (debug t) (indent defun))
  (let ((value (make-symbol "value"))
        (start (make-symbol "start"))
        (gcs (make-symbol "gcs"))
        (gc (make-symbol "gc")))
    `(let ((,gc gc-elapsed)
           (,gcs gcs-done)
           (,start (current-time))
           (,value (progn
                     ,@body)))
       (message "Benchmark: Elapsed time: %fs, %s%s%s"
                (float-time (time-since ,start))
                (cond ((eq ,action 'require)
                       "Required: '")
                      ((eq ,action 'load)
                       "Loaded: ")
                      (t (format "%s: " ,action)))
                ,feature
                (if (> (- gcs-done ,gcs) 0)
                    (format " (%fs in %d GCs)"
                            (- gc-elapsed ,gc)
                            (- gcs-done ,gcs))
                  ""))
       ;; Return the value of the body.
       ,value)))

;;; add use-package ":benchmark" keyword:

(defalias 'use-package-normalize/:benchmark 'use-package-normalize-predicate)

(defun use-package-handler/:benchmark (name _keyword arg rest state)
  "Wrap `use-package` with `benchmark-action` when `:benchmark t` is used."
  (let ((body (use-package-process-keywords name rest state)))
    (if arg
        `((use-package-benchmark--macro ',name 'use-package
            ,@body))
      body)))

(add-to-list 'use-package-keywords :benchmark)

;; add ":benchmark" to the default list of use-package keywords
;; (add-to-list 'use-package-defaults
;;              '(:benchmark '(t) use-package-benchmark-by-default))
(setq use-package-defaults
      (cons '(:benchmark '(t) use-package-benchmark-by-default)
            use-package-defaults))

;;; end
(provide 'use-package-benchmark)
