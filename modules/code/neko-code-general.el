
(use-package flycheck
  :defer t)

(use-package-local elec-pair
  :config
  (electric-pair-mode 1))

(use-package-local compile
  :custom
  (compilation-scroll-output t))

;; move elsewhere?
(global-prettify-symbols-mode 1)


;; TODO: consider adding lang to every filename?


(provide 'neko-code-general)
