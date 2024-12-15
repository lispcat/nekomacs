
;;; eshell ;;;
(use-package eshell
  :defer t
  :bind
  ("C-c p e" . eshell))

;;; eat ;;;

;; Diagram:
;; https://abode.karthinks.com/share/eat-modes.png
(use-package eat
  :defer t
  :config
  (setq eat-term-name "xterm-256color")
  (setq eat-kill-buffer-on-exit t)
  :bind
  ("C-c p p" . eat))
