
;;; Theme ;;;

(use-package emacs
  :config
  ;; (+load-theme 'ef-trio-dark t)
  ;; (+load-theme 'ef-rosa t)
  ;; (+load-theme 'doom-gruvbox t)
  ;; (+load-theme 'ef-bio t)
  ;; (+load-theme 'doom-moonlight t)
  ;; (+load-theme 'ef-dream t)
  ;; (+load-theme 'kaolin-temple t)
  ;; (+load-theme 'leuven-dark t)
  (mimi/set-random-theme))

;;; Anki :::

(use-package anki-editor)

;;; eshell ;;;
(use-package eshell
  :defer t
  :bind
  ("C-c p e" . eshell))

;;; Org ;;;

(use-package-local org
  ;; :after org
  :config
  ;; Failed experiment
  (setq org-list-full-item-re ;; TODO: add "- e.g." and "->" ?
	(concat
	 "^[ 	]*"
	 "\\(\\(?:[-+*/]\\|\\(?:[0-9]+\\|[A-Za-z]\\)[.)]\\)\\(?:[ 	]+\\|$\\)\\)"
	 "\\(?:\\[@\\(?:start:\\)?\\([0-9]+\\|[A-Za-z]\\)\\][ 	]*\\)?"
	 "\\(?:\\(\\[[ X-]\\]\\)\\(?:[ 	]+\\|$\\)\\)?"
	 "\\(?:\\(.*\\)[ 	]+::\\(?:[ 	]+\\|$\\)\\)?")))

;;; Workflow ;;;

(use-package-local org-agenda
  :after org
  :config
  (setq org-agenda-files
	(list "~/Notes/org/agenda.org"))
  ;; (define-key 'org-agenda-mode-map (kbd ")") 'org-agenda-todo)
  :bind (:map org-agenda-mode-map
	      (")" . 'org-agenda-todo)))

;;; Elfeed ;;;

(use-package elfeed
  :config
  ;; set `elfeed-feeds' to all files in `my/elfeed-feeds-dir'.
  (defvar my/elfeed-feeds-dir "~/feeds")
  (defun my/elfeed-feeds-update-var ()
    (interactive)
    (setq elfeed-feeds
	  (mapcar (lambda (s) (concat "file:" s))
		  (directory-files my/elfeed-feeds-dir t
				   directory-files-no-dot-files-regexp))))
  ;; run `my/elfeed-feeds-update-var' before running `elfeed-update'
  (advice-add #'elfeed-update :before #'my/elfeed-feeds-update-var))

;;; Keybinds ;;;

(use-package meow
  ;; :after meow
  :config
  (defun ri/meow-exit-all-and-save ()
    "When run, exit meow insert mode, exit snippet, then save buffer."
    (interactive)
    (meow-insert-exit)
    ;; (yas-abort-snippet)
    (save-buffer)
    (keyboard-quit))
  (defvar ri/meow-insert-default-modes
    '(vterm-mode
      eshell-mode)
    "Start these modes in meow-insert-mode.")
  (defvar ri/meow-SPC-ignore-list
    '(Info-mode
      gnus-summary-mode
      gnus-article-mode
      w3m-mode)
    "Disable meow-keypad in these modes.")
  
  ;; set some keys for insert-mode
  (meow-define-keys 'insert
    '("C-g" . meow-insert-exit)
    '("C-M-g" . ri/meow-exit-all-and-save))

  ;; start certain modes in insert-mode
  (dolist (mode ri/meow-insert-default-modes)
    (add-to-list 'meow-mode-state-list `(,mode . insert)))

  ;; disable meow-keypad (space key) on certain modes
  (defun ri/meow-SPC-ignore ()
    "When run, either run SPC for the current major-mode or meow-keypad."
    (interactive)
    ;; if t, appropriate to replace and run mode cmd.
    (if-let ((in-ignore-list?
	      (cl-some (lambda (mode)
			 (eq major-mode mode))
		       ri/meow-SPC-ignore-list))
	     (cmd-to-replace
	      (lookup-key (current-local-map) (kbd "SPC"))))
	(funcall cmd-to-replace)
      (meow-keypad)))
  (meow-motion-overwrite-define-key
   '("SPC" . ri/meow-SPC-ignore)))
