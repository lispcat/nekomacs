;; [[file:Init.org::*load no-littering][load no-littering:1]]
(defun init/no-littering ()
  ;; load
  (require 'no-littering)

  ;; variables
  (setq auto-save-default nil)       ; don't autosave all file buffers
  (setq backup-by-copying t)         ; safer backups
  (setq undo-tree-auto-save-history nil) ; TODO: is this value saved when undo-tree is loaded?

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
            ("." ,auto-save-dir t)))))
;; load no-littering:1 ends here

;; [[file:Init.org::*install a package manager][install a package manager:1]]
(defvar init/pkg-manager-installed nil)

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
    (load bootstrap-file nil 'nomessage))
  ;; use-package ":straight t" by default
  (setq straight-use-package-by-default t)
  ;; indicate that a package manager has been installed
  (setq init/pkg-manager-installed t))

(defun init/install-pkg-manager-package ()
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (unless package-archive-contents
    (package-refresh-contents))
  ;; use-package ":ensure t" by default
  (setq use-package-always-ensure t)
  ;; indicate that a package manager has been installed
  (setq init/pkg-manager-installed t))
;; install a package manager:1 ends here

;; [[file:Init.org::*necessary for Modules][necessary for Modules:1]]
(defun init/modules-dependencies ()
  ;; ensure that a package manager is installed
  (unless init/pkg-manager-installed
    (warn "In funcall `init/modules-dependencies', a package manager is not installed (`init/pkg-manager-installed' is nil), installing straight.el")
    (init/install-pkg-manager-straight))

  ;; enable use-package
  (require 'use-package)

  ;; libs
  (require 'use-package-universal) ; enable :fetch and :local keywords
  (require 'use-package-benchmark) ; benchmarking use-package invocations
  (require 'neko-defvar-improved)  ; provide `+defvar' macro

  ;; --- Packages: ---

  ;; todo: support evil leader key (has to be set in early-config?)
  (use-package general :fetch t
    :demand t
    :config
    (general-create-definer neko/leader-definer
      :prefix "C-c"))

  (use-package diminish :fetch t
    :demand t)

  (use-package which-key :fetch t
    :demand t
    :diminish which-key-mode
    :config
    (setq which-key-idle-delay 0.3)
    (which-key-mode 1)))
;; necessary for Modules:1 ends here

;; [[file:Init.org::*post-init][post-init:1]]
(defun init/post-init ()
  (add-hook 'emacs-startup-hook
            (lambda ()
              (message "*** Emacs loaded in %s seconds with %d garbage collections."
                       (emacs-init-time "%.2f")
                       gcs-done))))
;; post-init:1 ends here
