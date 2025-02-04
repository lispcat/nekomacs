
;; rainbow-mode (show hex colors)
(use-package rainbow-mode
  :hook prog-mode)

;; project.el mods
(use-package-local project
  :general-config
  (neko/leader-definer
    "p" project-prefix-map)
  
  :config
  (defun project-compile-interactive ()
    (declare (interactive-only compile))
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'project-compile)))
  (define-key project-prefix-map (kbd "C") #'project-compile-interactive))

;; yaml
(use-package yaml-mode
  :mode "\\.yml\\'")

;; nix
(use-package nix-mode)
