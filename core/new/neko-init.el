;; [[file:Init.org::*sane-defaults][sane-defaults:1]]
(defun init/sane-defaults ()
  (when (featurep 'native-compile)
    (setq native-comp-async-report-warnings-errors nil) ; Silence compiler warnings
    ))
;; sane-defaults:1 ends here

;; [[file:Init.org::*install-package-manager][install-package-manager:1]]
(defun init/install-pkg-manager-straight ()
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          (or (bound-and-true-p straight-base-dir)
              user-emacs-directory)))
        (bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(defun init/install-pkg-manager-package ()
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents)))
;; install-package-manager:1 ends here

;; [[file:Init.org::*install-use-package][install-use-package:1]]
(defun init/install-use-package-with-straight ()
  (unless (package-installed-p 'use-package)
    (straight-use-package 'use-package))
  (setq straight-use-package-by-default t))

(defun init/install-use-package-with-package ()
  (unless (package-installed-p 'use-package)
    (package-install 'use-package))
  (require 'use-package))
;; install-use-package:1 ends here

;; [[file:Init.org::*keyword - benchmark][keyword - benchmark:1]]
(defun init/use-package-kwd-benchmark ()
  (progn
    (require 'benchmark)
    (require 'use-package-core) ;; does this suffice?

  ;;; variable to enable keyword by default:

    ;; (defcustom use-package-benchmark-by-default t
    ;;   "Non-nil enables benchmarking on all use-package invocations."
    ;;   :type 'boolean
    ;;   :group 'use-package)

  ;;; add use-package ":benchmark" keyword:

    (defalias 'use-package-normalize/:benchmark 'use-package-normalize-predicate)

    (defun use-package-handler/:benchmark (name _keyword arg rest state)
      "Wrap the `use-package` declaration in `benchmark-progn` when `:benchmark t` is used."
      (let ((body (use-package-process-keywords name rest state)))
        (if arg
            `((+benchmark-action ',name 'use-package
  	        ,@body))
          body)))

    (add-to-list 'use-package-keywords :benchmark)

    ;; add ":benchmark" to the default list of use-package keywords
    (setq use-package-defaults
          (cons '(:benchmark
  	          '(t)
  	          ;; use-package-benchmark-by-default
  	          neko-benchmark
  	          )
  	        use-package-defaults))))
;; keyword - benchmark:1 ends here

;; [[file:Init.org::*use-package-local macro][use-package-local macro:1]]
(defun init/use-package-xtra-local ()
  (progn

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

    ;; (defmacro use-module (name &rest args)
    ;;       "A wrapper around use-package' that disables remotely fetching packages.
    ;; Used for loading modules/packages that are already installed."
    ;;       (declare (indent defun))
    ;;       (when (plist-member args :custom)
    ;;         (error "Cannot use :custom in use-module for %s. \
    ;; Long story, techincal reasons. Instead use :config and setq pls n tank u :3"
    ;; 	       name))
    ;;       `(use-package ,name ;; TODO: add support for custom specified package manager
    ;;          ,@(use-module--generate-keywords)
    ;;          ,@args))

    (defmacro use-package-local (name &rest args)
      "A wrapper around use-package' that disables remotely fetching packages.
  Used for loading modules/packages that are already installed."
      (declare (indent defun))
      `(use-package ,name ;; TODO: add support for custom specified package manager
         ,@(use-module--generate-keywords)
         ,@args))

    ;; set up font-lock syntax highlighting
    ;; (font-lock-add-keywords 'emacs-lisp-mode
    ;;     		    '(("(\\(use-module\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
    ;;     		       (1 font-lock-keyword-face)
    ;;     		       (2 font-lock-constant-face nil t))))
    (font-lock-add-keywords 'emacs-lisp-mode
  			    '(("(\\(use-package-local\\)\\_>[ \t']*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
  			       (1 font-lock-keyword-face)
  			       (2 font-lock-constant-face nil t))))
    ))
;; use-package-local macro:1 ends here

;; [[file:Init.org::*no-littering][no-littering:1]]
(defun init/no-littering ()
  (use-package no-littering
    :demand t
    :custom
    (auto-save-default nil) ; don't autosave all file buffers
    (backup-by-copying t) ; safer backups
    (undo-tree-auto-save-history nil) ; TODO: is this value saved when undo-tree is loaded?
    :config
    ;; Dont litter project folders with backup files
    (let ((backup-dir (no-littering-expand-var-file-name "backup/")))
      (make-directory backup-dir t)
      (setq backup-directory-alist
            `(("\\`/tmp/" . nil)
              ("\\`/dev/shm/" . nil)
              ("." . ,backup-dir))))
    ;; Tidy up auto-save files
    (let ((auto-save-dir (no-littering-expand-var-file-name "auto-save/")))
      (make-directory auto-save-dir t)
      (setq auto-save-file-name-transforms
            `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
               ,(concat (file-name-as-directory temporary-file-directory) "\\2") t)
              ("\\`/tmp\\([^/]*/\\)*\\(.*\\)\\'" "\\2")
              ("\\`/dev/shm\\([^/]*/\\)*\\(.*\\)\\'" "\\2")
              ("." ,auto-save-dir t))))))
;; no-littering:1 ends here

;; [[file:Init.org::*module dependencies][module dependencies:1]]
(defun init/module-dependencies ()
  ;; todo: support evil leader key (has to be set in early-config?)
  (use-package general
    :demand t
    :config
    (general-create-definer neko/leader-definer
      :prefix "C-c"))

  (use-package diminish
    :demand t)

  (use-package which-key
    :after diminish
    :demand t
    :diminish which-key-mode
    :config
    (setq which-key-idle-delay 0.3)
    (which-key-mode 1))
  )
;; module dependencies:1 ends here

;; [[file:Init.org::*post-init][post-init:1]]
(defun init/post-init ()
  (add-hook 'emacs-startup-hook
            (lambda ()
              (message "*** Emacs loaded in %s seconds with %d garbage collections."
                       (emacs-init-time "%.2f")
                       gcs-done))))
;; post-init:1 ends here

;; [[file:Init.org::*module-loading-macros][module-loading-macros:1]]
  ;;; Benchmarking:

(defmacro +benchmark-action (feature action &rest body)
  "A slight modification to the `benchmark-progn' macro.
    It now prints FEATURE when printing the time taken to eval BODY.
    ACTION can be either \\='require or \\='load, which prepends FEATURE with either
    \"Required: \" or \"Loaded: \". If neither match, print \\=`\",ACTION: \"."
  (declare (debug t) (indent defun))
  (let ((value (make-symbol "value"))
        (start (make-symbol "start"))
        (gcs (make-symbol "gcs"))
        (gc (make-symbol "gc")))
    `(let ((,gc gc-elapsed)
           (,gcs gcs-done)
           (,start (current-time))
           (,value (progn
                     ,@body)))
       (message "Benchmark: Elapsed time: %fs, %s%s%s"
                (float-time (time-since ,start))
                (cond ((eq ,action 'require)
                       "Required: '")
                      ((eq ,action 'load)
                       "Loaded: ")
                      (t (format "%s: " ,action)))
                ,feature
                (if (> (- gcs-done ,gcs) 0)
                    (format " (%fs in %d GCs)"
                            (- gc-elapsed ,gc)
                            (- gcs-done ,gcs))
                  ""))
       ;; Return the value of the body.
       ,value)))
  ;;; Commentary:

;; When running `defvar', if the variable is previously defined with `defvar',
;; its original value is not overwritten. However, this does not apply if
;; previously defined with `setq', or if `defvar' is run interactively.
;;
;; This package provides `+defvar', which unlike the former, never overwrites
;; the value if previously defined in any way.

    ;;; Code:

(defmacro +defvar (symbol &optional initvalue docstring)
  "The same as `defvar' but INITVALUE is only set if SYMBOL is unset.
    So if SYMBOL already exists with a value, it is not changed.
    But the DOCSTRING is set no matter the condition."
  (declare (indent defun)
           (doc-string 3))
  `(progn
     (defvar ,symbol ,(if (boundp symbol)
                          symbol
                        initvalue)
       ,docstring)))

(put '+defvar 'lisp-define-type 'var)
(put '+defvar 'edebug-form-spec '(symbolp &optional form stringp))

;; syntax highlighting:

(eval-after-load 'font-lock
  (font-lock-add-keywords
   'emacs-lisp-mode
   `((,(concat "(\\+defvar\\_>"                 ; match "(+defvar" and no more
               "[ \t']*"                ; match whitespace(s)/quote
               "\\(\\(?:\\sw\\|\\s_\\)+\\)?") ; match a symbol
      (1 'font-lock-variable-name-face nil t)))))


  ;;; Logging:

(defun +warn-error (type thing err)
  (display-warning type
                   (format "%s: %s" thing (error-message-string err))
                   :error))

(eval-after-load 'font-lock
  (font-lock-add-keywords
   'emacs-lisp-mode
   `(("(\\(\\+warn-error\\)\\_>"
      (1 'error)))))

  ;;; modules

(defun +load (file &optional noerror nomessage nosuffix must-suffix)
  "Run `load' with soft-error handling and optional benchmarking.
      Benchmarking is ran with `+benchmark-action'."
  (condition-case-unless-debug e
      (let ((body (lambda () (load file noerror nomessage nosuffix must-suffix))))
        (if neko-benchmark
            (+benchmark-action file 'load
              (funcall body))
          (progn
            (funcall body))))
    (error (+warn-error '+load file e))))

(defun +require (feature &optional filename noerror)
  "Run `require' with soft-error handling and optional benchmarking.
      Benchmarking is ran with `+benchmark-action'."
  (condition-case-unless-debug e
      (let ((body (lambda () (require feature filename noerror))))
        (if neko-benchmark
            (+benchmark-action feature 'require
              (funcall body))
          (progn
            (funcall body))))
    (error (+warn-error '+require feature e))))

(eval-after-load 'font-lock
  (font-lock-add-keywords
   'emacs-lisp-mode
   `((,(concat "(\\(\\+require\\)\\_>"   ; match "(+require" and no more
               "[ \t']*"                       ; match whitespace(s)/quote
               "\\(\\(?:\\sw\\|\\s_\\)+\\)?") ; match full symbol
      (1 font-lock-keyword-face)
      (2 font-lock-constant-face nil t)))))
;; module-loading-macros:1 ends here

;; [[file:Init.org::*new universal use-package keyword][new universal use-package keyword:1]]
(progn
  (message "DEBUG: Name: %s" name)
  (message "DEBUG: Keyword: %s" keyword)
  (message "DEBUG: Arg: %s" arg)
  (message "DEBUG: Rest: %s" rest)
  (message "DEBUG: filteredRest: %s" filtered-rest)
  (message "DEBUG: State: %s" state)
  (message "DEBUG: Body: %s" body))
;; new universal use-package keyword:1 ends here
