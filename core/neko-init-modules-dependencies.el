;; [[file:Init.org::*necessary for Modules][necessary for Modules:1]]
;;; --- do the bare minimum that's required for the modules --- ;;;

;; ensure that a package manager is installed
(unless init/pkg-manager-installed
  (warn (concat "In funcall `init/modules-dependencies', a package manager "
                "is not installed (`init/pkg-manager-installed' is nil), "
                "installing straight.el"))
  (init/install-pkg-manager-straight))

;; enable use-package
;; (straight-use-package 'use-package) ;; unnecessary
(require 'use-package)

;; libs
(require 'use-package-universal) ; enable :fetch and :local keywords
(require 'use-package-benchmark) ; benchmarking use-package invocations

(require 'neko-defvar-improved)  ; provide `+defvar' macro

;; install necessary packages with use-package

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


(provide 'neko-init-modules-dependencies)
;; necessary for Modules:1 ends here
