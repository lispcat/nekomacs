;; fzf-like minibuffer completion, keywords in any order
;; (https://github.com/oantolin/orderless)
;; TODO: read up on setting up for company and other packages.
(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))


(provide 'neko-vertico-orderless)
