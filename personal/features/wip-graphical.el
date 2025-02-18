
;;; Modeline ;;;
;; (use-package mood-line
;;   :config
;;   (mood-line-mode 1))

;;; Theme ;;;

(setq modus-themes-fringes nil)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  ;; (doom-themes-org-config)
  )

(use-package solaire-mode
  :demand t
  :config
  (solaire-global-mode +1))

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
  (neko/set-random-theme))

