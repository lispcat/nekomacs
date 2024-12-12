

;;; native compilation settings:

(when (featurep 'native-compile)
  ;; Silence compiler warnings
  (setq native-comp-async-report-warnings-errors nil))


(provide '20-good-defs)
