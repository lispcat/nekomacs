
;; eshell

(use-package eshell
  :defer t
  :general
  (neko/leader-definer
    "a e" 'eshell))

;; tweak eat

(use-package eat
  :defer t
  :config
  (setq eat-shell
	(concat (or explicit-shell-file-name
		    (getenv "ESHELL")
		    shell-file-name)
		" -c tmux")))

