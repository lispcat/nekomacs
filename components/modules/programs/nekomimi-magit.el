
(use-package magit
  ;; :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :defer t)

(mimi/leader-define-key
  "v" '(:ignore t :which-key "magit")
  "V" 'magit
  "vv" 'magit)


(provide 'nekomimi-magit)
