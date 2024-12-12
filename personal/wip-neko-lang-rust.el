
;; TODO: switch to lsp-mode, since less issues with lag and
;; more stable and more well-rounded and less likely to fuck up.

(use-package rustic
  :custom
  (rustic-cargo-use-last-stored-arguments t) ; ?
  :config
  (setq rustic-lsp-client 'eglot)
  (setq rustic-format-on-save nil))
