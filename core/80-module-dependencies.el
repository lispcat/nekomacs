
(use-package general
  :config
  (general-create-definer mimi/leader-define-key
    :prefix "C-c")
  (general-create-definer neko/leader-definer
    :prefix "C-c"))

(use-package diminish)

(use-package which-key
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-mode 1))


(provide '80-module-dependencies)
