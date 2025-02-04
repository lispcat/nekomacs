(defun neko/setup-markdown-mode ()
  ;; (visual-fill-column-mode 1)
  (display-line-numbers-mode 0))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode))
  :config
  ;; (setq markdown-command "marked")
  (add-hook 'markdown-mode-hook #'neko/setup-markdown-mode)
  (setq markdown-fontify-code-blocks-natively t))


(provide 'neko-lang-markdown)
