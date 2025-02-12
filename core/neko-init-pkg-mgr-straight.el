;; [[file:Init.org::*install a package manager][install a package manager:1]]
;;; --- install straight.el --- ;;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; use-package ":straight t" by default
(setq straight-use-package-by-default t)

;; indicate that a package manager has been installed
(setq init/pkg-manager-installed t)

(provide 'neko-init-pkg-mgr-straight)
;; install a package manager:1 ends here
