
;;; Vterm:

(use-package vterm
  :defer t
  :custom
  (vterm-buffer-name-string "vterm %s")
  (vterm-max-scrollback 10000)
  :bind (:map vterm-mode-map
	      ;; ("C-," . vterm-send-next-key)
	      ;; ("C--" . text-scale-decrease)
	      ;; ("C-+" . text-scale-increase)
	      ;; ("C-p" . vterm-send-up)
	      ;; ("C-n" . vterm-send-down)
	      )
  :config
  ;; Fixes vterm compilation on Guix System.
  ;; (https://www.reddit.com/r/GUIX/comments/11gzhyu/
  ;;  how_to_compile_the_vterm_module_from_emacs_and/)
  (progn
    ;; TODO: what to do about this.... and ri/is-guix-system...
    (defun ri/vterm-link-guix-library-on-compile (f &rest r)
      "Advice to replace compiling vterm with linking to just symlinking the guix library"
      (let* ((guix-vterm-lib "~/.guix-extra-profiles/emacs/emacs/lib/vterm-module.so")
	     (dest-directory (file-name-directory (locate-library "vterm.el" t)))
	     (dest-file (file-name-concat dest-directory "vterm-module.so")))
	(if (f-exists-p guix-vterm-lib)
	    ;; if non-symlink (compiled library) is at dest, delete
	    (unless (file-symlink-p dest-file)
	      (delete-file dest-file))
	  ;; create symlink
          (make-symbolic-link guix-library dest-file t)
          (message "DEBUG: vterm guix library %s doesn't exist, cant compile" guix-vterm-lib))))
    (if t ; TODO: gotta make this proper eventually
	(advice-add 'vterm-module-compile :around #'ri/vterm-link-guix-library-on-compile))))

(use-package multi-vterm
  :after vterm
  ;; :custom (multi-vterm-dedicated-window-height-percent 30)
  )

;;; Leader-key binds:

(mimi/leader-define-key
  "pt" 'vterm
  "pmm" 'multi-vterm-dedicated-toggle)

(provide 'nekomimi-vterm)
