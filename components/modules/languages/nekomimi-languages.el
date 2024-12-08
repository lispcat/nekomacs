
(use-package flycheck
  :defer t)

(use-package smartparens
  :diminish smartparens-mode
  :config
  (add-hook 'prog-mode-hook #'smartparens-mode))

;; TODO: consider adding lang to every filename?


(provide 'nekomimi-languages)
