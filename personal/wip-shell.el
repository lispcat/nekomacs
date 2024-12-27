
;;; eshell ;;;
(use-package eshell
  :defer t
  :general
  (neko/leader-definer
   "a e" 'eshell))

;;; eat ;;;

;; Diagram:
;; https://abode.karthinks.com/share/eat-modes.png
(use-package eat
  :defer t
  :config
  (setq eat-term-name "xterm-256color")
  (setq eat-kill-buffer-on-exit t)
  (setq eat-shell
	(concat (or explicit-shell-file-name
		    (getenv "ESHELL")
		    shell-file-name)
		" -c tmux"))
  :general
  (neko/leader-definer
    "a a" 'eat))
