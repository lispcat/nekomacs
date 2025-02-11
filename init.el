;;

(message "DEBUG: start of init")

;; gc
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold neko-after-init-gc-cons-threshold)))

;;; crash if unmet reqs
(let ((min-ver 29))
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

;;; add submodules to load-path
(add-to-list 'load-path (concat neko-root-dir "submodules/no-littering"))
(add-to-list 'load-path (concat neko-root-dir "submodules/use-package-universal"))
(add-to-list 'load-path (concat neko-root-dir "submodules/use-package-benchmark"))

(message "DEBUG: added load-paths")

;;; set user-emacs-directory to local dir
(setq user-emacs-directory neko-local-dir)

;; load module-loading macros (+require, +load, ...)
(require 'module-loading-macros)

;;; load core files (lexigraphically load all core files that are
;;; prefixed by two numbers.)

(message "DEBUG: before neko-init")

;; (+safe-progn
;;   (require 'neko-init))

(message "DEBUG: after neko-init")

;; (dolist (f neko/init-functions)
;;   (funcall f)
;;   (message "DEBUG: called %s" f))

;; (init/no-littering)
;; (init/install-pkg-manager-straight)
;; (init/modules-dependencies)
;; (init/post-init)

(let* ((dir neko-core-dir)
       (paths (sort (directory-files dir t "^[0-9][0-9]-.*$") #'string<)))
  (dolist (path paths)
    (when (file-regular-p path) ; Only load regular files, not directories
      (let ((file-stem-as-symbol (intern (file-name-base path))))
        (when (require file-stem-as-symbol nil t) ; non-nil if exists
          (require file-stem-as-symbol))))))

;;; load custom-file
(when (file-exists-p custom-file)
  (+safe-progn
    (load custom-file)))

(message "DEBUG: before 'neko-modules")

;; loads all the functions for the modules
(+safe-progn
  (require 'neko-modules))

(message "DEBUG: after 'neko-modules")

;;; loads main.el
(let ((file (file-name-concat neko-personal-dir "main.el")))
  (if file
      (+safe-progn
        (load file))
    (error "main.el in `neko-personal-dir' (%s) doesn't exist" file)))


;;; End
(message "Emacs initialized!")
