
;;; Install themes

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
  (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default)))

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
    (set-face-attribute 'fringe nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default))
    ;; mesg
    (message "Enabled theme: %s" chosen-theme)))

;;; Leader-key binds:

(neko/leader-definer
 "T" '(:ignore t :which-key "Themes")
 "Tt" '(+load-theme :which-key "load-theme")
 "Tr" '(neko/set-random-theme :which-key "set-random-theme")
 )

;;; (Note: actually setting a theme should be done after loading this file).


(provide 'neko-themes)
