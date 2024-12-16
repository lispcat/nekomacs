

(neko/leader-definer
  "k" 'kill-current-buffer
  "b" '(:ignore t :which-key "buffer")
  "bk" 'kill-current-buffer
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "bo" '(mode-line-other-buffer :which-key "last-buffer")
  "bb" 'switch-to-buffer
  "s" '(:ignore t :which-key "save")
  "ss" 'save-buffer
  )


(provide 'neko-buffers)
