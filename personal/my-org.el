
;;; Org ;;;

(use-package-local org
  :config
  (setq org-directory "~/Notes/org"))

(use-package-local org-tempo
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("scm" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("unix" . "src conf-unix"))
  (add-to-list 'org-structure-template-alist '("conf" . "src conf"))
  (add-to-list 'org-structure-template-alist '("clang" . "src c"))
  (add-to-list 'org-structure-template-alist '("gcc" . "src c"))
  (add-to-list 'org-structure-template-alist '("java" . "src java")))

(use-package toc-org
  :hook org-mode)

;;; Anki :::

(use-package anki-editor
  :defer t)

;;; Org-download ;;;

(use-package org-download
  :config
  (setq-default org-download-image-dir "_images"))
