
(use-package magit
  ;; :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :defer t
  :general
  (neko/leader-definer
    "v" 'magit))


(provide 'neko-magit)
