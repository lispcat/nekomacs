
;; TODO: add to guide: "(" to show details
(use-package-local dired
  :custom
  (dired-listing-switches "-Ahl --group-directories-first -X") ; -o is -l without groups
  (dired-auto-revert-buffer t) ; auto update file changes
  :config
  ;; hide details by default
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  ;; use trash if trash executable is found
  (when (executable-find "trash")
    (setq delete-by-moving-to-trash t))

  :general
  (neko/leader-definer
    "d" '(:ignore t :which-key "dired")
    "dd" 'find-file
    "dj" 'dired-jump))

(provide 'neko-dired)
