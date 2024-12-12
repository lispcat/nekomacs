
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  ("C-h h" . helpful-at-point)
  ("C-h H" . view-hello-file)	       ; command originally at "C-h h"
  ("C-h M" . which-key-show-major-mode)
  ("C-h E" . describe-keymap))

(provide 'neko-enhancements)
