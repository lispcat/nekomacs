
;; By default, the ESC key will actuate the Meta/Alt key.
;; The change below makes ESC work more like expected.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Shorten yes/no prompts to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; By default, emacs thinks two spaces after a period == a sentence.
;; The change below changes that to just one space.
(setq sentence-end-double-space nil)


(provide 'nekomimi-sane-defs)
