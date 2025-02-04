
;;; Functions:

(defun mimi/org-insert-subheading-respect-content ()
  "Insert new subheading after the current heading's body.
If in a list, inserts a new sublist after the current list."
  (interactive)
  (org-meta-return)
  (org-metaright))

(use-package org
  :custom
  (org-hide-emphasis-markers t) ; hide formatting chars (* / ~ = etc)
  ;; (org-src-preserve-indentation t) ; no space at front of code blocks
  (org-startup-indented t) ; indent headings and its body
  (org-startup-folded 'showall) ; default folding mode
  :bind (:map org-mode-map
	      ("C-M-<return>" . mimi/org-insert-subheading-respect-content)))

(use-package-local org-tempo
  :after org
  :general 
  (neko/leader-definer
    "o" '(:ignore t :which-key "org"))  :config
  ;; TODO: move most of these elsewhere, userside?
  ;; maybe in each prog-lang, `(eval-after-load 'org-tempo add to list)`
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(provide 'neko-org)
