
;; Docs: use M-SPC for separator
(use-package corfu
  :demand t
  :bind (:map corfu-map
              ;; ("C-j" . corfu-next)
              ;; ("C-k" . corfu-previous)
              ("TAB" . corfu-insert)
              ([tab] . corfu-insert)	; TODO: why repeat??
              ;; ("C-f" . corfu-insert)
	      )
  :custom
  (corfu-cycle t)		    ; cycle bottom/top
  (corfu-auto t)		    ; ?
  (corfu-preview-current nil)	    ; dont insert text while searching
  ;; (corfu-quit-at-boundary t)
  (corfu-quit-no-match t)		; quit if no matches

  :config
  (global-corfu-mode 1)

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

(provide 'nekomimi-corfu)

