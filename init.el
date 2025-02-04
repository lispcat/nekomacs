;;

;; gc
(add-hook 'after-init-hook
	  (lambda ()
	    (setq gc-cons-threshold neko-after-init-gc-cons-threshold)))

;;; crash if unmet reqs
(let ((min-ver 28))
  (when (< emacs-major-version min-ver)
    (error "Emacs version %s is not supported, muts be %s or higher" emacs-version min-ver)))

;;; add dirs to load-path
(defun add-subdirs-to-load-path (path)
  "Add PATH and all its subdirs to the `load-path'."
  (when path
    (let ((default-directory path))
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))))
(add-subdirs-to-load-path neko-core-dir)
(add-subdirs-to-load-path neko-modules-dir)
(add-subdirs-to-load-path neko-personal-dir)

;;; set user-emacs-directory to local dir
(setq user-emacs-directory neko-local-dir)

;;; load module-loading macros (+require, +load, ...)
(require 'module-loading-macros)

;;; load core files (lexigraphically load all core files that are
;;; prefixed by two numbers.)
(let* ((dir neko-core-dir)
       (paths (sort (directory-files dir t "^[0-9][0-9]-.*$") #'string<)))
  (dolist (path paths)
    (when (file-regular-p path) ; Only load regular files, not directories
      (let ((file-stem-as-symbol (intern (file-name-base path))))
	(when (require file-stem-as-symbol nil t) ; non-nil if exists
	  (+require file-stem-as-symbol))))))

;;; load custom-file
(when (file-exists-p custom-file)
  (+load custom-file))

;;; loads all the functions for the modules
(+require 'neko-modules)

;;; loads main.el
(let ((file (file-name-concat neko-personal-dir "main.el")))
  (if file
      (+load file)
    (error "main.el in `neko-personal-dir' (%s) doesn't exist" file)))


;;; End
(message "Emacs initialized!")
