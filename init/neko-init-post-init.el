;; [[file:Init.org::*post-init][post-init:1]]
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

(provide 'neko-init-post-init)
;; post-init:1 ends here
