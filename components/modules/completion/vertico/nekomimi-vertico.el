
;;; Vertico - a framework for minibuffer completion

;; savehist
;; consult
;; orderless
;; marginalia
;; embark
;; embark-consult
;; project.el
;; cape

;; ? : corfu, kind-icon, wgrep?, consult-dir, cape
;; ^ more at ~/code/cloned/daviwil-dots/.emacs.d/modules/dw-interface.el
;; TODO: vim keybinds for vertico completion shit (work on later) (also daviwil)
;; TODO: this module has high order priority (one less than keybinds)

;; a framework for minibuffer completion
;; (https://github.com/minad/vertico)
(use-package vertico
  :init
  (vertico-mode 1)
  ;; :custom
  ;; (vertico-scroll-margin 0) ; Different scroll margin
  ;; (vertico-count 20) ; Show more candidates
  ;; (vertico-resize t) ; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ; Enable cycling for `vertico-next/previous'
  )

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)
  
  ;; Emacs 28 and newer: hide commands in M-x that do not work in the current mode.
  ;; (setq read-extended-command-predicate #'command-completion-default-include-p)
  
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))


(provide 'nekomimi-vertico)
