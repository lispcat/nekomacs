;; [[file:Modules.org::*essential tweaks][essential tweaks:1]]
(defun m/essential-tweaks ()
  ;; By default, the ESC key will actuate the Meta/Alt key.
  ;; The change below makes ESC work more like expected.
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

  ;; Shorten yes/no prompts to y/n
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; By default, emacs thinks two spaces after a period equals a sentence.
  ;; The change below changes that to just one space.
  (setq sentence-end-double-space nil))
;; essential tweaks:1 ends here

;; [[file:Modules.org::*Buffers][Buffers:1]]
(defun m/buffers ()
  ;; revert buffer when its file is changed on the filesystem
  (use-package autorevert :local t
    :diminish autorevert-mode
    :init
    (global-auto-revert-mode 1)
    :custom
    (global-auto-revert-non-file-buffers t)
    (auto-revert-interval 5))

  (defalias 'neko/last-selected-buffer 'mode-line-other-buffer)

  (neko/leader-definer
    "k" 'kill-current-buffer
    "b" '(:ignore t :which-key "buffer")
    "bk" 'kill-current-buffer
    "bn" 'next-buffer
    "bp" 'previous-buffer
    "bo" '(neko/last-selected-buffer :which-key "last-buffer")
    "bb" 'switch-to-buffer
    "bs" 'save-buffer))
;; Buffers:1 ends here

;; [[file:Modules.org::*History][History:1]]
(defun m/history ()
  ;; remember recent files
  (use-package recentf :fetch t
    :local t
    :hook (emacs-startup . recentf-mode))
  ;; go to previous location in file when reopening
  (use-package saveplace :fetch t
    :local t
    :config
    (save-place-mode 1))
  ;; persist minibuffer history over restarts
  (use-package savehist :fetch t
    :local t
    :config
    (savehist-mode 1)))
;; History:1 ends here

;; [[file:Modules.org::*Windows][Windows:1]]
(defun m/windows ()
  (use-package ace-window :fetch t
    :custom
    (aw-scope 'frame)
    (aw-background nil)
    (aw-keys '(?a ?s ?d ?f ?j ?k ?l)) ; TODO: Note: override for non-qwerty!
    ;; (aw-dispatch-always t)
    :bind
    ("M-o" . ace-window)        ; Improved window switching with "M-o"
    )

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
    ))
;; Windows:1 ends here

;; [[file:Modules.org::*Dired][Dired:1]]
(defun m/dired ()
  ;; TODO: add to guide: "(" to show details
  (use-package dired :local t
    :custom
    (dired-listing-switches "-Ahl --group-directories-first -X") ; -o is -l without groups
    (dired-auto-revert-buffer t) ; auto update file changes
    :config
    ;; hide details by default
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)
    ;; use trash if trash executable is found
    (when (executable-find "trash")
      (setq delete-by-moving-to-trash t))
    :general
    (neko/leader-definer
      "d" '(:ignore t :which-key "dired")
      "dd" 'find-file
      "dj" 'dired-jump)))
;; Dired:1 ends here

;; [[file:Modules.org::*Files][Files:1]]
(defun m/files ()
  (defun neko/open-neko-personal-dir ()
    (interactive)
    (dired neko-personal-dir))
  ;;
  ;; Set leader-key binds:
  (neko/leader-definer
    "f" '(:ignore t :which-key "files")
    "ff" 'find-file
    "fp" 'neko/open-neko-personal-dir))
;; Files:1 ends here

;; [[file:Modules.org::*Helpful][Helpful:1]]
(defun m/helpful ()
  (use-package helpful :fetch t
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
    ("C-h E" . describe-keymap)))
;; Helpful:1 ends here

;; [[file:Modules.org::*easy editing of sudo files][easy editing of sudo files:1]]
(defun m/auto-sudoedit ()
  ;; sudoedit
  (use-package auto-sudoedit))
;; easy editing of sudo files:1 ends here

;; [[file:Modules.org::*Vertico][Vertico:1]]
(defun m/vertico ()
  ;; ? : corfu, kind-icon, wgrep?, consult-dir, cape
  ;; ^ more at ~/code/cloned/daviwil-dots/.emacs.d/modules/dw-interface.el
  ;; TODO: vim keybinds for vertico completion shit (work on later) (also daviwil)
  ;;
  ;; a framework for minibuffer completion
  ;; (https://github.com/minad/vertico)
  (use-package vertico :fetch t
    :init
    (vertico-mode 1)
    ;; :custom
    ;; (vertico-scroll-margin 0) ; Different scroll margin
    ;; (vertico-count 20) ; Show more candidates
    ;; (vertico-resize t) ; Grow and shrink the Vertico minibuffer
    ;; (vertico-cycle t) ; Enable cycling for `vertico-next/previous'
    )
  ;; A few more useful configurations...
  (use-package emacs :local t
    :init
    ;; Support opening new minibuffers from inside existing minibuffers.
    (setq enable-recursive-minibuffers t)
    ;;
    ;; Emacs 28 and newer: hide commands in M-x that do not work in the current mode.
    ;; (setq read-extended-command-predicate #'command-completion-default-include-p)
    ;;
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
    ;;
    ;; Do not allow the cursor in the minibuffer prompt
    (setq minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)))
;; Vertico:1 ends here

;; [[file:Modules.org::*Cape][Cape:1]]
(defun m/cape ()
  (use-package cape :fetch t
    :demand t
    ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
    ;; Press C-c p ? to for help.
    :bind ("M-+" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
    ;; Alternatively bind Cape commands individually.
    ;; :bind (("C-c p d" . cape-dabbrev)
    ;;        ("C-c p h" . cape-history)
    ;;        ("C-c p f" . cape-file)
    ;;        ...)
    :init
    ;; Add to the global default value of `completion-at-point-functions' which is
    ;; used by `completion-at-point'.  The order of the functions matters, the
    ;; first function returning a result wins.  Note that the list of buffer-local
    ;; completion functions takes precedence over the global list.
    (add-hook 'completion-at-point-functions #'cape-dabbrev)
    (add-hook 'completion-at-point-functions #'cape-file)
    (add-hook 'completion-at-point-functions #'cape-elisp-block)
    ;; (add-hook 'completion-at-point-functions #'cape-history)
    ;; ...
    ;; (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
    ;; ...
    ))
;; Cape:1 ends here

;; [[file:Modules.org::*Consult][Consult:1]]
(defun m/consult ()

  (use-package consult :fetch t
    :bind (;; C-c bindings in `mode-specific-map'
           ("C-c M-x" . consult-mode-command)
           ;; ("C-c )" . consult-kmacro)

           ;; C-x bindings in `ctl-x-map'
           ("C-x M-:" . consult-complex-command) ;; repeat-complex-command
           ("C-x b" . consult-buffer)	       ;; switch-to-buffer
           ("C-x 4 b" . consult-buffer-other-window) ;; switch-to-buffer-other-window
           ("C-x 5 b" . consult-buffer-other-frame) ;; switch-to-buffer-other-frame
           ("C-x t b" . consult-buffer-other-tab)	;; switch-to-buffer-other-tab
           ("C-x r b" . consult-bookmark)		;; bookmark-jump
           ("C-x p b" . consult-project-buffer) ;; project-switch-to-buffer
           ("C-x p C-b" . consult-project-buffer) ;; project-switch-to-buffer

           ;; Custom M-# bindings for fast register access
           ("M-#" . consult-register-store)
           ;; ("C-M-#" . consult-register)
           ("C-M-#" . consult-register-load)

           ;; Other custom bindings
           ("M-y" . consult-yank-pop) ;; yank-pop
           ([remap Info-search] . consult-info)

           ;; M-g bindings in `goto-map'
           ("M-g e" . consult-compile-error)
           ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
           ("M-g g" . consult-goto-line)	 ;; goto-line
           ("M-g M-g" . consult-goto-line) ;; goto-line
           ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
           ("M-g m" . consult-mark)
           ("M-g k" . consult-global-mark)
           ("M-g i" . consult-imenu)
           ("M-g I" . consult-imenu-multi)
           ("M-g O" . consult-org-heading)

           ;; M-s bindings in `search-map'
           ("M-s d" . consult-find) ;; Alternative: consult-fd
           ("M-s c" . consult-locate)
           ("M-s g" . consult-grep)
           ("M-s G" . consult-git-grep)
           ("M-s r" . consult-ripgrep)
           ("M-s l" . consult-line)
           ("M-s L" . consult-line-multi)
           ("M-s k" . consult-keep-lines)
           ("M-s u" . consult-focus-lines)
           ("M-s M" . consult-man)	; T for terminal
           ("M-s I" . consult-info)

           ;; Isearch integration
           ("M-s e" . consult-isearch-history)
           :map isearch-mode-map
           ("M-e" . consult-isearch-history)   ;; isearch-edit-string
           ("M-s e" . consult-isearch-history) ;; isearch-edit-string
           ("M-s l" . consult-line) ;; Needed by: consult-line to detect isearch
           ("M-s L" . consult-line-multi)	;; Needed by: consult-line to detect isearch

           ;; Minibuffer history
           :map minibuffer-local-map
           ("M-s" . consult-history) ;; next-matching-history-element
           ("M-r" . consult-history) ;; previous-matching-history-element
           )
    :general
    (neko/leader-definer
      "s" search-map))

  ;; used to go to a file in a bookmarked dir n stuff (one ex)
  (use-package consult-dir :fetch t
    :general
    (neko/leader-definer
      "fd" 'consult-dir)

    :bind (("C-x C-d" . consult-dir)	; default?
           :map vertico-map
           ("C-x C-d" . consult-dir)
           ("C-x C-j" . consult-dir-jump-file))
    ;; :custom
    ;; (consult-dir-project-list-function nil)
    )

  ;; TODO: do i even need to do this here?
  ;; - oh wait i do since the other module might overwrite...
  ;; - but the issue is that it never gets set if those modules
  ;; are never loaded...
  ;; - maybe in the other module files, only set those functions
  ;; if another bind isnt already there?
  ;; - is it possible to do eval-after-load 'thing OR after init?
  ;; and throw away the other autoload once one succeeds?

  (defmacro mi/eval-now-and-after-load (feature &rest body)
    "Eval BODY, then if FEATURE is not loaded, eval BODY again after FEATURE loaded."
    (declare (indent defun))
    (let ((f (cadr feature)))
      `(progn
         ;; always eval now
         ,@body
         ;; if feature not loaded, eval again after load feature
         ,(unless (featurep f)
            `(eval-after-load ',f
               (lambda () ,@body))))))

  (mi/eval-now-and-after-load 'neko-themes
    (neko/leader-definer
      "Tt" 'consult-theme))

  (mi/eval-now-and-after-load 'neko-buffers
    (neko/leader-definer
      "bb" 'consult-buffer))

  (mi/eval-now-and-after-load 'neko-dired
    (neko/leader-definer
      "fr" 'consult-recent-file))

  (neko/leader-definer
    "fm" 'consult-bookmark)
  )
;; Consult:1 ends here

;; [[file:Modules.org::*Corfu][Corfu:1]]
;; Docs: use M-SPC for separator
(defun m/corfu ()
  (use-package corfu :fetch t
    :demand t
    :bind (:map corfu-map
                ;; ("C-j" . corfu-next)
                ;; ("C-k" . corfu-previous)
                ("TAB" . corfu-insert)
                ([tab] . corfu-insert)  ; TODO: why repeat??
                ("RET" . nil)
                ;; ("C-f" . corfu-insert)
                ("S-TAB" . corfu-next)
                ("S-M-TAB" . corfu-previous)
                )
    :custom
    (corfu-cycle t)                 ; cycle bottom/top
    (corfu-auto t)                  ; ?
    (corfu-preview-current nil)     ; dont insert text while searching
    ;; (corfu-quit-at-boundary t)
    (corfu-quit-no-match t)             ; quit if no matches

    :config
    (global-corfu-mode 1)

    (defun corfu-enable-in-minibuffer ()
      "Enable Corfu in the minibuffer if `completion-at-point' is bound."
      (when (where-is-internal #'completion-at-point (list (current-local-map)))
        ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
        (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                    corfu-popupinfo-delay nil)
        (corfu-mode 1)))
    (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)))
;; Corfu:1 ends here

;; [[file:Modules.org::*Embark][Embark:1]]
(defun m/embark ()
  (use-package embark :fetch t
    :bind
    (("C-." . embark-act)
     ("C-;" . embark-dwim)
     ;; ("C-h B" . embark-bindings)
     )
    :init
    ;; use embark for showing command prefix help
    (setq prefix-help-command #'embark-prefix-help-command)

    ;; Show the Embark target at point via Eldoc. You may adjust the
    ;; Eldoc strategy, if you want to see the documentation from
    ;; multiple providers. Beware that using this can be a little
    ;; jarring since the message shown in the minibuffer can be more
    ;; than one line, causing the modeline to move up and down:

    ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
    ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
    :config
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none))))))
;; Embark:1 ends here

;; [[file:Modules.org::*Embark-Consult][Embark-Consult:1]]
(defun m/embark-consult ()
  (use-package embark-consult :fetch t
    :after (embark consult)
    :hook
    (embark-collect-mode . consult-preview-at-point-mode)))
;; Embark-Consult:1 ends here

;; [[file:Modules.org::*Marginalia][Marginalia:1]]
(defun m/marginalia ()
  (use-package marginalia :fetch t
    :bind
    (:map minibuffer-local-map     ("M-A" . marginalia-cycle))
    (:map completion-list-mode-map ("M-A" . marginalia-cycle))
    :init
    (marginalia-mode 1)))		; force-load immediately
;; Marginalia:1 ends here

;; [[file:Modules.org::*Orderless][Orderless:1]]
(defun m/orderless ()
  (use-package orderless :fetch t
    :custom
    ;; Configure a custom style dispatcher (see the Consult wiki)
    ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
    ;; (orderless-component-separator #'orderless-escapable-split-on-space)
    (completion-styles '(orderless basic))
    (completion-category-defaults nil)
    (completion-category-overrides '((file (styles partial-completion))))))
;; Orderless:1 ends here

;; [[file:Modules.org::*Yasnippet][Yasnippet:1]]
(defun m/yasnippet ()
  ;; TODO: this is set up for eglot only, not lsp-mode

  ;; https://stackoverflow.com/questions/72601990/how-to-show-suggestions-for-yasnippets-when-using-eglot

  ;; TODO: move elsewhere?:
  (use-package yasnippet :fetch t
    :diminish yas-minor-mode
    :hook (prog-mode . yas-minor-mode)
    :config
    (yas-reload-all))

  ;; (use-package yasnippet-snippets :fetch t
  ;;   :after yasnippet)

  ;; yasnippet completion-at-point support
  ;; (use-package yasnippet-capf :fetch t
  ;;   :after cape yasnippet
  ;;   :config
  ;;   ;; enable yasnippet-capf everywhere
  ;;   (progn
  ;;     (add-to-list 'completion-at-point-functions #'yasnippet-capf))
  ;;   ;; integrate yasnippet-capf with eglot completion
  ;;   ;; (progn
  ;;   ;;   (defun mi/eglot-capf-with-yasnippet ()
  ;;   ;;     (setq-local completion-at-point-functions
  ;;   ;;                 (list
  ;;   ;;                     (cape-capf-super
  ;;   ;;                      #'yasnippet-capf
  ;;   ;;                      #'eglot-completion-at-point))))
  ;;   ;;   (with-eval-after-load 'eglot
  ;;   ;;     (add-hook 'eglot-managed-mode-hook #'mi/eglot-capf-with-yasnippet)))
  ;;   )
  )
;; Yasnippet:1 ends here

;; [[file:Modules.org::*IDE essentials][IDE essentials:1]]
(defun m/ide-essentials ()
  (setq-default indent-tabs-mode nil)
  (setq tab-always-indent 'complete) ; test

  (use-package compile :local t
    :custom
    (compilation-scroll-output t))

  (use-package flycheck :fetch t
    :hook prog-mode
    :config
    (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))))
;; IDE essentials:1 ends here

;; [[file:Modules.org::*eglot][eglot:1]]
(defun m/eglot ()
  (use-package eglot :fetch t
    :defer t))
;; eglot:1 ends here

;; [[file:Modules.org::*lsp-mode][lsp-mode:1]]
(defun m/lsp-mode ()
  (use-package lsp-mode :fetch t
    :defer t
    :commands (lsp lsp-deferred)
    ;; bind "C-c l" to lsp-command-map
    :custom (lsp-keymap-prefix "C-c l")
    :general-config
    (neko/leader-definer
      "l" lsp-command-map)
    ;; lsp-command-map which-key integration
    :hook (lsp-mode . lsp-enable-which-key-integration))

  ;; TODO: move this to corfu ?
  ;; if corfu is installed
  ;; (https://github.com/minad/corfu/wiki#configuring-corfu-for-lsp-mode)
  (use-package lsp-mode :fetch t
    :defer t
    :after corfu
    :hook (lsp-completion-mode . my/lsp-mode-setup-completion)
    :init
    (defvar my/lsp-mode-setup-completion-type '(flex))
    (with-eval-after-load 'orderless
      (setq my/lsp-mode-setup-completion-type '(orderless)))
    (defun my/lsp-mode-setup-completion ()
      (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
            my/lsp-mode-setup-completion-type))
    :custom (lsp-completion-provider :none)))
;; lsp-mode:1 ends here

;; [[file:Modules.org::*essentials][essentials:1]]
(defun m/lang-essentials ()
  (use-package elec-pair :local t
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
    (electric-pair-mode 1)))
;; essentials:1 ends here

;; [[file:Modules.org::*elisp][elisp:1]]
(defun m/lang-elisp ()
  (use-package rainbow-delimiters :fetch t
    :hook emacs-lisp-mode))
;; elisp:1 ends here

;; [[file:Modules.org::*java (eglot)][java (eglot):1]]
(defun m/lang-java-eglot ()
  (use-package eglot-java :fetch t
    :defer t))
;; java (eglot):1 ends here

;; [[file:Modules.org::*java (lsp-mode)][java (lsp-mode):1]]
(defun m/lang-java-lsp-mode ()
  (use-package lsp-java :fetch t
    :config
    (add-hook 'java-mode-hook #'lsp)))
;; java (lsp-mode):1 ends here

;; [[file:Modules.org::*lisp for advanced lispers][lisp for advanced lispers:1]]
(defun m/lang-lisp-advanced ()
  (use-package paredit :fetch t
    :hook emacs-lisp-mode scheme-mode ; TODO: do this better
    ))
;; lisp for advanced lispers:1 ends here

;; [[file:Modules.org::*markdown][markdown:1]]
(defun m/lang-markdown ()
  (use-package markdown-mode :fetch t
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode))
    :config
    (defun neko/setup-markdown-mode ()
      ;; (visual-fill-column-mode 1)
      (display-line-numbers-mode 0))

    ;; (setq markdown-command "marked")
    (add-hook 'markdown-mode-hook #'neko/setup-markdown-mode)
    (setq markdown-fontify-code-blocks-natively t)))
;; markdown:1 ends here

;; [[file:Modules.org::*rust][rust:1]]
(defun m/lang-rust ()
  (use-package rustic :fetch t
    :defer t
    :custom
    (rustic-cargo-use-last-stored-arguments t) ; ?
    :config
    ;; (setq rustic-lsp-client 'lsp-mode)
    (setq rustic-format-on-save nil)))
;; rust:1 ends here

;; [[file:Modules.org::*scheme][scheme:1]]
(defun m/lang-scheme ()
  (use-package rainbow-delimiters :fetch t
    :hook scheme-mode)

  (use-package scheme-mode :local t
    :mode "\\.sld\\'")

  (use-package geiser :fetch t
    :defer t
    :custom
    (geiser-default-implementation 'guile)
    (geiser-active-implementations '(guile))
    (geiser-implementations-alist '(((regexp "\\.scm$") guile))))

  (use-package geiser-guile :fetch t
    :after geiser)
  )
;; scheme:1 ends here

;; [[file:Modules.org::*org][org:1]]
(defun m/org ()
  (defun neko/org-insert-subheading-respect-content ()
    "Insert new subheading after the current heading's body.
  If in a list, inserts a new sublist after the current list."
    (interactive)
    (org-meta-return)
    (org-metaright))

  (use-package org :fetch t
    :custom
    (org-hide-emphasis-markers t) ; hide formatting chars (* / ~ = etc)
    (org-startup-indented t)       ; indent headings and its body
    (org-startup-folded 'showall)  ; default folding mode
    (org-tags-column -60)          ; column where tags are indented to
    (org-src-preserve-indentation t) ; remove annoying leading whitespace in code blocks
    (org-return-follows-link t)      ; RET can open links
    (org-src-window-setup 'current-window) ; edit code blocks in the same window
    :general (neko/leader-definer
               "o" '(:ignore t :which-key "org"))
    :bind (:map org-mode-map
                ("C-M-<return>"
                 . neko/org-insert-subheading-respect-content)))

  (use-package org-tempo :local t
    :after org
    :config
    ;; TODO: move most of these elsewhere, userside?
    ;; maybe in each prog-lang, `(eval-after-load 'org-tempo add to list)`
    (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))))
;; org:1 ends here

;; [[file:Modules.org::*org-agenda][org-agenda:1]]
(defun m/org-agenda ()
  (use-package org-agenda :local t
    :after org
    :general
    (neko/leader-definer
      "oa" 'org-agenda)))
;; org-agenda:1 ends here

;; [[file:Modules.org::*Meow][Meow:1]]
(defun m/meow ()
  (use-package meow
    :custom
    (meow-replace-state-name-list
     '((normal . "<N>")
       (motion . "<M>")
       (keypad . "<K>")
       (insert . "<I>")
       (beacon . "<B>")))))
;; Meow:1 ends here

;; [[file:Modules.org::*meow-qwerty][meow-qwerty:1]]
(defun m/meow-qwerty ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore))

  (meow-global-mode 1))
;; meow-qwerty:1 ends here

;; [[file:Modules.org::*meow-dvp][meow-dvp:1]]
(defun m/meow-dvp ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvp)
  (meow-motion-overwrite-define-key
   ;; custom keybinding for motion state
   '("<escape>" . ignore)
   '("t" . "p") ;; improved solution? (access Motion "t" with "SPC t")
   )
  (meow-leader-define-key
   '("t" . "H-t")
   ;; '("p" . "H-p")
   ;; '("u" . ctl-x-map)
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   ;; make S-<num> easier to hit with DVP by using symbols.
   '("*" . meow-expand-0)
   '("=" . meow-expand-9)
   '("!" . meow-expand-8)
   '("[" . meow-expand-7)
   '("]" . meow-expand-6)
   '("{" . meow-expand-5)
   '("+" . meow-expand-4)
   '("}" . meow-expand-3)
   '(")" . meow-expand-2)
   '("(" . meow-expand-1)
   '("1" . digit-argument)
   '("2" . digit-argument)
   '("3" . digit-argument)
   '("4" . digit-argument)
   '("5" . digit-argument)
   '("6" . digit-argument)
   '("7" . digit-argument)
   '("8" . digit-argument)
   '("9" . digit-argument)
   '("0" . digit-argument)
   ;; symbols
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '(":" . meow-goto-line) ;; moved from "Q" and "E"
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   ;; basic letters
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   ;; '("d" . ri/meow-delete-or-kill)
   '("d" . meow-delete) ; i want "d" to delete char after meow-prev/next-word, so dont use former
   '("D" . meow-backward-delete)
   '("e" . meow-line)
   ;; '("E" . meow-goto-line) ;; removed, since ":" for it works
   '("f" . meow-find)
   '("F" . meow-search) ;; moved from "s" ("s" is used for movement)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   ;; H Directional key moved to the bottom
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-join)
   '("k" . meow-kill)
   '("l" . meow-till)
   ;; '("m" . meow-mark-word) ;; swap with w, next-word (because "b"/"m" is easy for mvmnt)
   ;; '("M" . meow-mark-symbol) ;; swap with W, next-symbol (because "b"/"m" is easy for mvmnt)
   '("m" . meow-next-word)   ;; moved from "w", mark-word
   '("M" . meow-next-symbol) ;; moved from "W", mark-symbol
   ;; N Directional key moved to the bottom
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . meow-quit)
   '("Q" . ri/quit-temp-window)
   ;; '("Q" . meow-goto-line) ;; move to " : "
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   ;; '("s" . meow-search) ;; move to F, replace with directional keys
   ;; S Directional key moved to the bottom
   ;; T Directional key moved to the bottom
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   ;; '("w" . meow-next-word) ;; swap with m, mark-word/symbol
   ;; '("W" . meow-next-symbol)
   '("w" . meow-mark-word)   ;; moved from "m", mark-word
   '("W" . meow-mark-symbol) ;; moved from "M", mark-symbol
   '("x" . meow-save)
   '("X" . meow-sync-grab)
   '("y" . meow-yank)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("/" . ri/scroll-down-half-page) ;; new keys
   '("?" . ri/scroll-up-half-page)   ;; new keys
   ;; '("<escape>" . ignore)

   ;; Directional keys:

   ;; <-  ^  v  ->
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("t" . meow-prev)
   '("T" . meow-prev-expand)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("s" . meow-right)
   '("S" . meow-right-expand)

   ;; ^  <-  v  ->
   ;; '("h" . meow-prev)
   ;; '("H" . meow-prev-expand)
   ;; '("t" . meow-left)
   ;; '("T" . meow-left-expand)
   ;; '("n" . meow-next)
   ;; '("N" . meow-next-expand)
   ;; '("s" . meow-right)
   ;; '("S" . meow-right-expand)

   ;; ^  /  <-  ->  v
   ;; '("h" . meow-left)
   ;; '("H" . meow-left-expand)
   ;; '("t" . meow-right)
   ;; '("T" . meow-right-expand)
   ;; '("n" . meow-prev)
   ;; '("N" . meow-prev-expand)
   )

  (meow-global-mode 1))
;; meow-dvp:1 ends here

;; [[file:Modules.org::*avy][avy:1]]
(defun m/avy ()
  ;; avy
  (use-package avy
    :general
    (neko/leader-definer
      "j" 'avy-goto-char)))
;; avy:1 ends here

;; [[file:Modules.org::*spellcheck][spellcheck:1]]
(defun m/spellcheck ()
  ;; spellchecking
  (use-package jinx
    :hook (org-mode markdown-mode text-mode)
    :bind (("M-$" . jinx-correct)
           ("C-M-$" . jinx-languages))))
;; spellcheck:1 ends here

;; [[file:Modules.org::*theme][theme:1]]
(defun m/theme ()
  ;; Install themes

  ;; (use-package doom-themes)
  (use-package kaolin-themes)
  (use-package ef-themes)

    ;;; Function: `load-theme' but fixed theme-bleeding issue.

  (defun +load-theme (theme &optional no-confirm no-enable)
    "Prevent `load-theme' from having theme bleeding issues."
    (interactive
     (list
      (intern (completing-read "Load custom theme: "
                               (mapcar #'symbol-name
                                       (custom-available-themes))))
      nil nil))
    ;; disable all enabled themes
    (mapc #'disable-theme custom-enabled-themes)
    ;; enable theme
    (if (custom-theme-p theme)
        (enable-theme theme)
      (load-theme theme :no-confirm))
    ;; remove fringes
    ;; (set-face-attribute 'fringe nil
    ;;                     :foreground (face-foreground 'default)
    ;;                     :background (face-background 'default))
    )

    ;;; Function: sets a random theme.

  (defun neko/set-random-theme ()
    (interactive)
    (let* ((available-themes (custom-available-themes))
           (current-theme (car custom-enabled-themes))
           (themes-except-current (remove current-theme available-themes))
           (chosen-theme (nth (random (length themes-except-current))
                              themes-except-current)))
      ;; disable all enabled themes
      (mapc #'disable-theme custom-enabled-themes)
      ;; enable randomly chosen theme
      (if (custom-theme-p chosen-theme)
          (enable-theme chosen-theme)
        (load-theme chosen-theme :no-confirm))
      ;; remove fringes
      ;; (set-face-attribute 'fringe nil
      ;;                     :foreground (face-foreground 'default)
      ;;                     :background (face-background 'default))
      ;; mesg
      (message "Enabled theme: %s" chosen-theme)))

    ;;; Leader-key binds:

  (neko/leader-definer
    "T" '(:ignore t :which-key "Themes")
    "Tt" '(+load-theme :which-key "load-theme")
    "Tr" '(neko/set-random-theme :which-key "set-random-theme")
    )

    ;;; (Note: actually setting a theme should be done after loading this file).
  )
;; theme:1 ends here

;; [[file:Modules.org::*transparency][transparency:1]]
(defun m/transparency ()
  (defun neko/native-transparency-supported? ()
    (if (version<= "29" emacs-version)
        t
      (message "Native transparency is not supported.")
      nil))

  (defun neko/toggle-transparency ()
    (interactive)
    (when (neko/native-transparency-supported?)
      (let ((alpha (frame-parameter nil 'alpha-background)))
        (set-frame-parameter
         nil 'alpha-background
         (if (eql (cond ((numberp alpha) alpha)
                        ((numberp (cdr alpha)) (cdr alpha))
                        ;; Also handle undocumented (<active> <inactive>) form.
                        ((numberp (cadr alpha)) (cadr alpha)))
                  100)
             neko-transparency-value
           100)))))

  (defun neko/set-transparency (value)
    "Sets the transparency of the frame window. 0=transparent/100=opaque"
    (interactive "nTransparency Value 0 - 100 opaque: ")
    (when (neko/native-transparency-supported?)
      (set-frame-parameter (selected-frame) 'alpha-background value))))
;; transparency:1 ends here

;; [[file:Modules.org::*line numbers][line numbers:1]]
(defun m/line-numbers ()
  ;; list of programming modes to disable line-numbers on
  (defvar neko/display-line-numbers-exclude '())

  ;; enable line-numbers on programming modes
  (add-hook 'prog-mode-hook
            (lambda ()
              (unless (memq major-mode neko/display-line-numbers-exclude)
                (display-line-numbers-mode 1)))))
;; line numbers:1 ends here

;; [[file:Modules.org::*line wrap][line wrap:1]]
(defun m/line-wrap ()
  (global-visual-line-mode 1)
  (diminish 'visual-line-mode) ; hide "Wrap" in mode-line
  )
;; line wrap:1 ends here

;; [[file:Modules.org::*mode-line tweaks][mode-line tweaks:1]]
(defun m/mode-line-tweaks ()
  ;; show column # on modeline
  (column-number-mode 1))
;; mode-line tweaks:1 ends here

;; [[file:Modules.org::*doom-modeline][doom-modeline:1]]
(defun m/doom-modeline ()
  (use-package doom-modeline :fetch t
    :init
    (doom-modeline-mode 1)
    ;; :config
    ;; (setq doom-modeline-modal-icon nil)
    ))
;; doom-modeline:1 ends here

;; [[file:Modules.org::*scroll][scroll:1]]
(defun m/scroll ()
  ;; Improve scroll
  (use-package emacs :local t
    :custom
    ;; (auto-window-vscroll nil) ; TODO: what does this do?
    (scroll-preserve-screen-position t) ; keep point in same position while scrolling
    (scroll-conservatively 101) ; dont move cursor to center while scrolling
    (scroll-margin 2)		; scroll margin of one line
    (mouse-wheel-scroll-amount
     '(2				; faster vscroll speed
       ((shift) . hscroll)		; S-<scroll> for hscroll
       ((meta) . nil)			; M-<scroll> for PgUp/PgDn
       ((control) . text-scale)		; C-<scroll> for zoom
       ((control meta) . global-text-scale))) ; C-M-<scroll> for global zoom
    (mouse-wheel-scroll-amount-horizontal 2)  ; faster hscroll speed
    ))
;; scroll:1 ends here

;; [[file:Modules.org::*eat][eat:1]]
(defun m/term-eat ()
  (use-package eat :fetch t
    :defer t
    :config
    (setq eat-term-name "xterm-256color")
    (setq eat-kill-buffer-on-exit t)
    :general
    (neko/leader-definer
      "a a" 'eat)))
;; eat:1 ends here

;; [[file:Modules.org::*git client][git client:1]]
(defun m/magit ()
  (use-package magit :fetch t
    ;; :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
    :defer t
    :general
    (neko/leader-definer
      "v" 'magit)))
;; git client:1 ends here

;; [[file:Modules.org::*pdf reader][pdf reader:1]]
(defun m/pdf-tools ()
  (use-package pdf-tools :fetch t
    :init
    (pdf-loader-install))) ; On demand loading, leads to faster startup time
;; pdf reader:1 ends here

;; [[file:Modules.org::*server][server:1]]
(defun m/server ()
  (use-package server :local t
    :config
    ;; start server at first startup
    (defun ne/start-server-if-not-running ()
      (unless (or (processp server-process)
                  (server-running-p))
        (server-start)
        (message "Emacsclient Server started!")))
    (add-hook 'after-init-hook #'ne/start-server-if-not-running))

  (neko/leader-definer
    "q" 'delete-frame
    "Q" 'save-buffers-kill-emacs))
;; server:1 ends here

;; [[file:Modules.org::*ending][ending:1]]
(provide 'neko-modules)
;; ending:1 ends here
