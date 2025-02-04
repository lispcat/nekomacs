
;;; Improved window switching with "M-o"

(use-package ace-window
  :custom
  (aw-scope 'frame)
  (aw-background nil)
  (aw-keys '(?a ?s ?d ?f ?j ?k ?l)) ; TODO: Note: override for non-qwerty!
  ;; (aw-dispatch-always t)
  :bind
  ("M-o" . ace-window))

;;; Leader-key binds

(neko/leader-definer
  "w" '(:ignore t :which-key "window")
  "wd" 'delete-window
  "w+" 'balance-windows
  "wa" 'balance-windows-area
  ;; split window
  "wv" 'split-window-horizontally
  "ws" 'split-window-vertically
  ;; select window directionally
  "wp" '(windmove-up    :which-key "select up")
  "wn" '(windmove-down  :which-key "select down")
  "wf" '(windmove-right :which-key "select right")
  "wb" '(windmove-left  :which-key "select left")
  ;; misc
  "wm" 'switch-to-minibuffer
  )


(provide 'neko-windows)
