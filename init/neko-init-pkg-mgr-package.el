;; [[file:Init.org::*install a package manager][install a package manager:2]]
;;; --- set up package.el --- ;;;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
;; use-package ":ensure t" by default
(setq use-package-always-ensure t)

;; indicate that a package manager has been installed
(setq init/pkg-manager-installed t)

(provide 'neko-init-pkg-mgr-package)
;; install a package manager:2 ends here
