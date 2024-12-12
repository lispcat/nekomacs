

;; Useful annotations in minibuffer completions
;; (https://github.com/minad/marginalia)
(use-package marginalia
  ;; marginalia-cycle: show different amounts of info in minibuffer
  :bind
  (:map minibuffer-local-map
	("M-A" . marginalia-cycle))
  (:map completion-list-mode-map
	("M-A" . marginalia-cycle))
  :init
  ;; must be loaded in :init (enable immediately, force load)
  (marginalia-mode 1))

(provide 'neko-marginalia)
