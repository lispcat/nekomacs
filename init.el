;;

(message "start of init.el")

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
(add-subdirs-to-load-path neko-init-dir)
(add-subdirs-to-load-path neko-modules-dir)
(add-subdirs-to-load-path neko-personal-dir)

;;; add submodules to load-path
(add-to-list 'load-path (concat neko-root-dir "submodules/no-littering"))

;;; set user-emacs-directory to local dir
(setq user-emacs-directory neko-local-dir)

;; whether a package manager is installed
(defvar init/pkg-manager-installed nil)

(defun neko/init-require-process ()
  (dolist (lib neko/init-require)
    (require lib)))

(condition-case-unless-debug e
    (neko/init-require-process)
  (error
   (if (not (yes-or-no-p (concat "Error in init when processing `neko/init-require':"
                                 "\n" (error-message-string e) "\n"
                                 "Re-try with `neko/init-require--default'?")))
       (signal (car e) (cdr e))
     (setq neko/init-require neko/init-require--default)
     (neko/init-require-process))))

;;; load custom-file
(when (file-exists-p custom-file)
  (+safe-progn
    (load custom-file)))

;; loads all the functions for the modules
(+safe-progn
  (require 'neko-modules))

;;; loads main.el
(let ((file (file-name-concat neko-personal-dir "main.el")))
  (if file
      (+safe-progn
        (load file))
    (error "main.el in `neko-personal-dir' (%s) doesn't exist" file)))


;;; End
(message "Emacs initialized!")
