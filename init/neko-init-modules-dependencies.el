;; [[file:Init.org::*necessary for Modules][necessary for Modules:1]]
;;; --- do the bare minimum that's required for the modules --- ;;;

;; ensure that a package manager is installed
(unless init/pkg-manager-installed
  (if (y-or-n-p (concat "In loading `neko-init-modules-dependencies', "
                        "the `init/pkg-manager-installed' variable is nil."
                        "Fall back to straght.el instead?"))
      (init/install-pkg-manager-straight)
    (error "The `init/pkg-manager-installed' was nil when loading `neko-init-modules-dependencies'.
Consider tweaking the `neko/init-require' variable.")))

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
  :fetch t
  :demand t
  :config
  (general-create-definer neko/leader-definer
    :prefix "C-c"))

(use-package diminish
  :fetch t
  :demand t)

(use-package which-key
  :fetch t
  :demand t
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-mode 1))


(provide 'neko-init-modules-dependencies)
;; necessary for Modules:1 ends here
