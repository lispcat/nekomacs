
;;; history

;; TODO: consider moving some of these to neko-history.el?

;; TODO: create a custom nekomimi-lazy-load-hook for clarity?

;; track recently opened files
(use-package-local recentf
  :hook (emacs-startup . recentf-mode)) ; enable after startup

;; go to previous location in file when reopening
(use-package-local saveplace
  :config
  (save-place-mode 1))

;; persist minibuffer history over restarts
(use-package-local savehist
  :config
  (savehist-mode 1))

;;; Dired

;; update dired and file buffers when changed in filesystem
(use-package-local autorevert
  :diminish autorevert-mode
  :custom
  (global-auto-revert-non-file-buffers t)
  (auto-revert-interval 5))


(provide 'neko-file-tweaks)
