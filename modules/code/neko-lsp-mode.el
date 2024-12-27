(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-c l")
  :defer t
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :general-config
  (neko/leader-definer
    "l" lsp-command-map))

;; if corfu is installed
;; (https://github.com/minad/corfu/wiki#configuring-corfu-for-lsp-mode)
(use-package lsp-mode
  :defer t
  :after corfu
  :hook (lsp-completion-mode . my/lsp-mode-setup-completion)
  :init
  (defvar my/lsp-mode-setup-completion-type '(flex))
  (with-eval-after-load 'orderless
    (setq my/lsp-mode-setup-completion-type '(orderless)))
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          my/lsp-mode-setup-completion-type))
  :custom (lsp-completion-provider :none))

(provide 'neko-lsp-mode)
