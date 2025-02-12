
;;; Modify keybinds for the Dvorak Programmer layout

(use-package dired
  :local t
  :after dired
  :bind (:map dired-mode-map
              ("h" . dired-up-directory)
              ("s" . dired-find-file)
              ("r" . dired-sort-toggle-or-edit)))

(use-package ace-window
  :local t
  :after ace-window
  :config
  (setq aw-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))
