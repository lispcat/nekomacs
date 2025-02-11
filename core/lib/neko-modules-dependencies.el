
;; --- Packages: ---

;; todo: support evil leader key (has to be set in early-config?)
;; (straight-use-package 'general)
;; (general-create-definer neko/leader-definer
;;   :prefix "C-c")

(use-package general
  ;; :fetch t
  :demand t
  :config
  ;; (require 'general)
  (message "DEBUG: MEOWWW")
  (general-create-definer neko/leader-definer
    :prefix "C-c"))

;; (straight-use-package 'diminish)

(use-package diminish
  ;; :fetch t
  :demand t)

;; (straight-use-package 'which-key)
;; (setq which-key-idle-delay 0.3)
;; (which-key-mode 1)

(use-package which-key
  ;; :fetch t
  :demand t
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-mode 1))


(provide 'neko-modules-dependencies)
