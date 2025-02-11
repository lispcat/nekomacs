;;; TODO: this is set up for eglot only, not lsp-mode

;; https://stackoverflow.com/questions/72601990/how-to-show-suggestions-for-yasnippets-when-using-eglot
;; TODO: move elsewhere?:
(use-package yasnippet
  :diminish yas-minor-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

;; yasnippet completion-at-point support
(use-package yasnippet-capf
  :after cape yasnippet
  :config
  ;; enable yasnippet-capf everywhere
  (progn
    (add-to-list 'completion-at-point-functions #'yasnippet-capf))
  ;; integrate yasnippet-capf with eglot completion
  ;; (progn
  ;;   (defun mi/eglot-capf-with-yasnippet ()
  ;;     (setq-local completion-at-point-functions
  ;;                 (list
  ;; 		   (cape-capf-super
  ;; 		    #'yasnippet-capf
  ;; 		    #'eglot-completion-at-point))))
  ;;   (with-eval-after-load 'eglot
  ;;     (add-hook 'eglot-managed-mode-hook #'mi/eglot-capf-with-yasnippet)))
  )


(provide 'neko-vertico-yasnippet)
