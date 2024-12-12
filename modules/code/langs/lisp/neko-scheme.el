
(use-package-local scheme-mode
  :mode "\\.sld\\'")

(use-package geiser
  :custom
  (geiser-default-implementation 'guile)
  (geiser-active-implementations '(guile))
  (geiser-implementations-alist '(((regexp "\\.scm$") guile))))

(use-package geiser-guile
  :after geiser)


(provide 'neko-scheme)
