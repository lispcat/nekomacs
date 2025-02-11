
;; todo: support evil leader key (has to be set in early-config?)
(use-package general
  :demand t
  :config
  (general-create-definer neko/leader-definer
    :prefix "C-c"))

(use-package diminish
  :demand t)

(use-package which-key
  :after diminish
  :demand t
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-mode 1))


(provide '80-module-dependencies)
