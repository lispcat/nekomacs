;;

;; gc
(add-hook 'after-init-hook
	  (lambda ()
	    (setq gc-cons-threshold nekomimi-after-init-gc-cons-threshold)))

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
(add-subdirs-to-load-path nekomimi-components-dir)
(add-subdirs-to-load-path nekomimi-personal-dir)

;;; set user-emacs-directory to local dir
(setq user-emacs-directory nekomimi-local-dir)

;;; load module-loading macros (+require, +load, ...)
(require 'module-loading-macros)

;;; load necessary components (lexigraphically load all components in
;;; components/init/ that are prefixed by two numbers.)
(let* ((dir (concat nekomimi-components-dir "init/"))
       (paths (sort (directory-files dir t "^[0-9][0-9]-.*$") #'string<)))
  (dolist (path paths)
    (when (file-regular-p path) ; Only load regular files, not directories
      (let ((file-stem-as-symbol (intern (file-name-base path))))
	(when (require file-stem-as-symbol nil t) ; non-nil if exists
	  (+require file-stem-as-symbol))))))

;;; load custom-file
(when (file-exists-p custom-file)
  (+load custom-file))

;;; load my-components-config.el file
(let ((file nekomimi-modules-config))
  (when (file-exists-p file)
    (+load file)))

;;; load all in personal-dir recursively and lexigraphically
;;; (excludes special-config-dir and things beginning with "_").
(let* ((dir nekomimi-personal-dir)
       (excluded-dir nekomimi-special-config-dir)
       (paths (directory-files-recursively dir "^[^_].*\\.el$")))
  (dolist (path paths)
    (unless (string-match-p (regexp-quote excluded-dir) path)
      (let ((file-as-str path))
	(+load file-as-str)))))

;;; End
(message "Emacs initialized!")
(put 'upcase-region 'disabled nil)
