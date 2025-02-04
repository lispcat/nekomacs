
(use-package fontawesome
  :defer t)

;; load if using vertico
(use-package fontawesome
  :defer t
  :after vertico
  :config
  ;; vertico variant
  (defun vertico-fontawesome ()
    (interactive)
    (require 'vertico)
    (insert
     (cdr
      (assoc
       (completing-read "Font awesome: " (fontawesome--construct-candidates))
       (fontawesome--construct-candidates)))))
  ;; autoload (load package when function is called):
  :commands vertico-fontawesome)
