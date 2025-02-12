
;;; This config file loads at the end of `early-init.el', but
;;; right before `early-config.el' which lies in the
;;; `neko-special-config-dir'.

;;; This config file is useful if you wanna change a variable
;;; before `early-config.el' is loaded. For example, if you wanna
;;; change the path at which `early-config.el' is loaded from, or
;;; the location of any of its parent directories
;;; (`neko-personal-dir', `neko-special-config-dir', etc).

(setq neko-personal-dir
      "~/.emacs.d/personal"
      ;; "~/.emacs.d/personal-template"
      )

