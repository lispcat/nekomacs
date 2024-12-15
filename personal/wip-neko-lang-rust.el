
;; TODO: read these:
;; https://robert.kra.hn/posts/rust-emacs-setup/
;; https://github.com/emacs-rustic/rustic

(use-package rustic
  :custom
  (rustic-cargo-use-last-stored-arguments t) ; ?
  :config
  ;; (setq rustic-lsp-client 'lsp-mode)
  (setq rustic-format-on-save nil))
