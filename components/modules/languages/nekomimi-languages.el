
(use-package flycheck
  :defer t)

;; (use-package smartparens
;;   :diminish smartparens-mode
;;   :config
;;   (add-hook 'prog-mode-hook #'smartparens-mode))


(use-package-local elec-pair
  :config
  (electric-pair-mode 1))

;; TODO: consider adding lang to every filename?


(provide 'nekomimi-languages)
