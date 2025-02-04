
(use-package-local org-agenda
  :after org
  :general
  (neko/leader-definer
    "oa" 'org-agenda))

(provide 'neko-org-agenda)
