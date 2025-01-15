
;; move elsewhere?
(global-prettify-symbols-mode 1)

;; nice keybinds for navigation

(global-set-key (kbd "M-p") (kbd "M-- 1 C-v"))
(global-set-key (kbd "M-n") (kbd "M-- 1 M-v"))

;; visible bell (kinda nice)
;; (setq visible-bell t)

;; move elsewhere
(global-set-key (kbd "M-C-s") 'consult-line)
(global-set-key (kbd "C-s") 'consult-line)

;;; direnv/envrc

(use-package direnv
  :config
  (direnv-mode))

;; (use-package envrc
;;   :hook (after-init . envrc-global-mode))

;;; magit
(with-eval-after-load 'magit
  (defun magit-apply-patch-with-reject (file)
    "Apply a patch FILE with `git apply --reject`."
    (interactive "fPatch file: ")
    (magit-run-git "apply" "--reject" "--whitespace=fix" "--recount"
		   (expand-file-name file))))


;;
(use-package avy
  :bind ("C-c j" . avy-goto-char))
