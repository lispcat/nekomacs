(use-package lsp-mode
  ;; defer
  :defer t
  :commands (lsp lsp-deferred)
  ;; bind "C-c l" to lsp-command-map
  :custom (lsp-keymap-prefix "C-c l")
  :general-config
  (neko/leader-definer
    "l" lsp-command-map)
  ;; lsp-command-map which-key integration
  :hook (lsp-mode . lsp-enable-which-key-integration))

;; TODO: move this to corfu ?
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

(provide 'neko-code-lsp-mode)
