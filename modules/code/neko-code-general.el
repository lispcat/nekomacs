
(use-package flycheck
  :defer t)

(use-package-local elec-pair
  :config
  (electric-pair-mode 1))

(use-package compile
  :custom
  (compilation-scroll-output t))

;; TODO: consider adding lang to every filename?


(provide 'neko-code-general)
