
;; rainbow-mode (show hex colors)
(use-package rainbow-mode
  :hook prog-mode)

;; project.el mods
(use-package-local project
  :config
  (defun project-compile-interactive ()
    "Run `compile' in the project root."
    (declare (interactive-only compile))
    (interactive)
    (let ((default-directory (project-root (project-current t)))
          (compilation-buffer-name-function
           (or project-compilation-buffer-name-function
	       compilation-buffer-name-function))
	  (current-prefix-arg '(4)))	; universal-argument
      (call-interactively #'compile))
    ;; select comint window
    (select-window (catch 'done
		     (dolist (win (window-list))
		       (with-current-buffer (window-buffer win)
			 (when (eq major-mode 'comint-mode)
			   (throw 'done win))))))
    (end-of-buffer)
    (meow-insert-mode)			; TODO: specific to meow mode
    )
  (define-key project-prefix-map (kbd "C") #'project-compile-interactive))

;; yaml
(use-package yaml-mode
  :mode "\\.yml\\'")

;; nix
(use-package nix-mode)
