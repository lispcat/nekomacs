
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode)
  :hook
  (scala-mode . (lambda () (setq prettify-symbols-alist
			    scala-prettify-symbols-alist))))
