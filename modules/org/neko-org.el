
;;; Functions:

(defun mimi/org-insert-subheading-respect-content ()
  "Insert new subheading after the current heading's body.
If in a list, inserts a new sublist after the current list."
  (interactive)
  (org-meta-return)
  (org-metaright))

(use-package-local org
  :custom
  (org-hide-emphasis-markers t) ; hide formatting chars (* / ~ = etc)
  ;; (org-src-preserve-indentation t) ; no space at front of code blocks
  (org-startup-indented t) ; indent headings and its body
  (org-startup-folded 'showall) ; default folding mode
  :bind (:map org-mode-map
	      ("C-M-<return>" . mimi/org-insert-subheading-respect-content)))

(use-package-local org-tempo
  :after org
  :config
  ;; TODO: move most of these elsewhere, userside?
  ;; maybe in each prog-lang, `(eval-after-load 'org-tempo add to list)`
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("scm" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("unix" . "src conf-unix"))
  (add-to-list 'org-structure-template-alist '("conf" . "src conf"))
  (add-to-list 'org-structure-template-alist '("clang" . "src c"))
  (add-to-list 'org-structure-template-alist '("gcc" . "src c"))
  (add-to-list 'org-structure-template-alist '("java" . "src java")))

(mimi/leader-define-key
 "o" '(:ignore t :which-key "org"))


(provide 'neko-org)
