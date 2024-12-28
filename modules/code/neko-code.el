(use-package-local compile
  :custom
  (compilation-scroll-output t))

(use-package-local elec-pair
  :config
  ;; disable "<" pair expansion
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local electric-pair-inhibit-predicate
                          `(lambda (c)
                             (if (char-equal c ?<)
                                 t
                               (,electric-pair-inhibit-predicate c))))))
  ;; global
  (electric-pair-mode 1))

(use-package flycheck
  :defer t)

(provide 'neko-code)
