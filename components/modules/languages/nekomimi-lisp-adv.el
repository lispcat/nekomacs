
(use-package paredit
  :hook emacs-lisp-mode scheme-mode ; TODO: do this better
  :bind (:map paredit-mode-map
	      ("M-s" . nil)	   ; conflicts with consult search map
	      ))


(provide 'nekomimi-lisp-adv)
