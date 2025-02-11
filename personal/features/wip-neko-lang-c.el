
(use-package cc-mode
  :local t
  :demand t
  :hook ((c-mode . lsp)
         (c-mode . (lambda () (setq-local lsp-idle-delay 0.1))))
  :config
  (add-to-list 'c-default-style '(c-mode . "cc-mode"))
  (with-eval-after-load 'project
    (define-key c-mode-map (kbd "<f8>")
                (lambda () (interactive)
                  (let   ;; universal-argument
                      ((current-prefix-arg '(4)))
                    (call-interactively 'project-compile))))))
