
;;; Fonts

(defvar my-font-alist
  `((hack . "Hack")
    (tamzenPL-16
     . "-Misc-TamzenForPowerline-regular-normal-normal-*-16-*-*-*-c-80-iso10646-1")
    (ttyp0-17   .   "-UW  -Ttyp0-regular-normal-normal-*-17-*-*-*-m-*-iso8859-1")
    (ttyp0-17-b .        "-UW-Ttyp0-bold-normal-normal-*-17-*-*-*-c-90-iso8859-1")
    (ttyp0-16   .   "-UW  -Ttyp0-regular-normal-normal-*-16-*-*-*-m-*-iso8859-1")
    (ttyp0-16-i .   "-UW  -Ttyp0-regular-italic-normal-*-16-*-*-*-m-*-iso10646-1")
    (gb-16 . "-AW-Greybeard 16px-regular-normal-normal-*-16-*-*-*-c-80-iso10646-1")))

;; font config

;; (defun my-fontconfig ()
;;   ;; default
;;   (set-face-attribute 'default nil :font (alist-get 'tamzenPL-16 my-font-alist))
;;   ;; italic, bitmap exception
;;   (set-face-attribute 'italic  nil :font (alist-get 'ttyp0-16-i  my-font-alist)))

(defun my-fontconfig ()
  ;; default
  (set-face-attribute 'default nil :font (alist-get 'ttyp0-16 my-font-alist)))

;; (defun my-fontconfig ()
;;   ;; default
;;   (set-face-attribute 'default nil :font (alist-get 'gb-16 my-font-alist)))

(my-fontconfig)

;; hack to fix bitmap fonts on emacsclient frames
(add-hook 'server-after-make-frame-hook #'my-fontconfig)

;; variable pitch font






;;; Misc

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


;; origami ?

(use-package origami)


;; whitespace mode

(use-package whitespace
  :config
  (defun my/prog-mode-whitespace ()
    (setq whitespace-style '(face trailing tabs tab-mark))
    ;; (setq whitespace-style '(trailing tabs tab-mark))
    (whitespace-mode 1))
  :hook ((prog-mode . my/prog-mode-whitespace)
         (org-mode  . my/prog-mode-whitespace)
         (text-mode . my/prog-mode-whitespace)))



