
;; consult - navigation commands
;; (https://github.com/minad/consult)
(use-package consult
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
	 ))

;; used to go to a file in a bookmarked dir n stuff (one ex)
(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)	; default?
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  ;; :custom
  ;; (consult-dir-project-list-function nil)
  )
(mimi/leader-define-key
  "fd" 'consult-dir)


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
  (mimi/leader-define-key
    "Tt" 'consult-theme))

(mi/eval-now-and-after-load 'neko-buffers
  (mimi/leader-define-key
    "bb" 'consult-buffer))

(mi/eval-now-and-after-load 'neko-dired
  (mimi/leader-define-key
    "fr" 'consult-recent-file))

(mimi/leader-define-key
  "fm" 'consult-bookmark)


(provide 'neko-consult)
