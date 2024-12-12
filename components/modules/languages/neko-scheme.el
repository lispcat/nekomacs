
(use-package geiser
  :mode ("\\.sld\\'" . scheme-mode)
  :hook (scheme-mode . flycheck-mode) ; move elsewhere?
  :custom
  (geiser-default-implementation 'guile)
  (geiser-active-implementations '(guile))
  (geiser-implementations-alist '(((regexp "\\.scm$") guile))))

(use-package geiser-guile
  :after geiser)


(provide 'neko-scheme)
