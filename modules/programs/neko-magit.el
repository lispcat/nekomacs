
(use-package magit
  ;; :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :defer t)

(neko/leader-definer
  "v" '(:ignore t :which-key "magit")
  "V" 'magit
  "vv" 'magit)


(provide 'neko-magit)
