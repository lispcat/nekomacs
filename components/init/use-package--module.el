
(+defvar use-module-localizing-keywords
  '((straight . :straight)
    (package  . :ensure)
    (elpaca   . :elpaca))
  "An alist of (FEATURE . KEYWORD) to disable fetching for package managers.")

(defun use-module--generate-keywords ()
  (apply #'append
	 (mapcar (lambda (pair)
		   (let ((feature (car pair))
			 (keyword (cdr pair)))
		     (when (featurep feature)
		       `(,keyword nil))))
		 use-module-localizing-keywords)))

(defmacro use-module (name &rest args)
  "A wrapper around use-package' that disables remotely fetching packages.
Used for loading modules/packages that are already installed."
  (declare (indent defun))
  (when (plist-member args :custom)
    (error "Cannot use :custom in use-module for %s. \
Long story, techincal reasons. Instead use :config and setq pls n tank u :3"
	   name))
  `(use-package ,name ;; TODO: add support for custom specified package manager
     ,@(use-module--generate-keywords)
     ,@args))

(defmacro use-package-local (name &rest args)
  "A wrapper around use-package' that disables remotely fetching packages.
Used for loading modules/packages that are already installed."
  (declare (indent defun))
  `(use-package ,name ;; TODO: add support for custom specified package manager
     ,@(use-module--generate-keywords)
     ,@args))

;; set up font-lock syntax highlighting
(font-lock-add-keywords 'emacs-lisp-mode
			'(("(\\(use-module\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
			   (1 font-lock-keyword-face)
			   (2 font-lock-constant-face nil t))))
(font-lock-add-keywords 'emacs-lisp-mode
			'(("(\\(use-package-local\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
			   (1 font-lock-keyword-face)
			   (2 font-lock-constant-face nil t))))

(provide 'use-package--module)
