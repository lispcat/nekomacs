
;; TODO: add to guide: "(" to show details
(use-package-local dired
  :custom
  (dired-listing-switches "-a -h -l --group-directories-first") ; -o is -l without groups
  (dired-auto-revert-buffer t) ; auto update file changes
  :config
  ;; hide details by default
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  ;; use trash if trash executable is found
  (when (executable-find "trash")
    (setq delete-by-moving-to-trash t)))

(neko/leader-definer
  "d" '(:ignore t :which-key "dired")
  "dd" 'find-file
  "dj" 'dired-jump)

(provide 'neko-dired)
