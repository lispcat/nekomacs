

;;;;;; General important:

;;; Variables

;;; Prevent package.el from auto-loading
(setq package-enable-at-startup nil)

;; less garbage collection at startupg
(setq gc-cons-threshold (* 50 1000 1000))

;; dont run byte-compiled code if outdated to .el file
(setq load-prefer-newer t)

;; Silence compiler warnings
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors nil))

;; remove unneeded UI elements
(setq default-frame-alist
      '((tool-bar-lines . 0)            ; disable tool bar
        (menu-bar-lines . 0)            ; disable menu bar
        (vertical-scroll-bars)          ; disable vertical scroll bar
        (drag-internal-border . t)
        (internal-border-width . 13) ; box border around buffer+modeline (creates gap) (prev: 15)
        (fullscreen . maximized)     ; TODO: ???
        (left-fringe)           ; set left fringe
        (right-fringe)          ; set right fringe
        ))
(setq tool-bar-mode nil                         ; disable tool bar
      menu-bar-mode nil                         ; disable menu bar
      scroll-bar-mode nil               ; disable vertical scroll bar
      )

;; prevent toolbar setup from running (optimization)
(advice-add 'tool-bar-setup :override #'ignore)
;; undo previous change so user can manually enable tool-bar later
(add-hook 'emacs-startup-hook
          (lambda ()
            (advice-remove 'tool-bar-setup #'ignore)
            (when tool-bar-mode (tool-bar-setup))))

;;; Paths:

(defconst neko-root-dir
  (abbreviate-file-name
   (file-name-directory
    (file-truename
     (or load-file-name buffer-file-name)))))
(defvar neko-init-dir (concat neko-root-dir "init/"))
(defvar neko-modules-dir (concat neko-root-dir "modules/"))
(defvar neko-personal-dir (concat neko-root-dir "personal/"))
(defvar neko-local-dir (concat neko-root-dir "local/"))

(setq custom-file (concat neko-local-dir "custom-vars.el"))

;;; User-side variables:

;; benchmark
(defvar use-package-benchmark-by-default nil)
;; increase gc freq after init
(defvar neko-after-init-gc-cons-threshold (* 2 1000 1000))
;; transparency
(defvar neko-transparency-value 100) ; (100 = no transparency).

;; which init functions to load
(defvar neko/init-require--default
  '(neko-init-no-littering
    neko-init-pkg-mgr-straight
    neko-init-modules-dependencies
    neko-init-post-init))
(defvar neko/init-require neko/init-require--default
  "A list of libraries to load with `require' at init.")

;;; User-side functions:

;; enable transparency at startup (TODO: would this fuck up later set-frame-parameter?)
(defun nekomini-enable-init-transparency ()
  (unless (assoc 'alpha-background default-frame-alist)
    (add-to-list 'default-frame-alist (cons 'alpha-background neko-transparency-value))))

;;; Fixes

;; changes the eln-cache dir to be inside a subdir for cleanliness
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" neko-local-dir))))

;;; Load startup-config.el and early-config.el:

(defmacro +safe-progn (&rest body)
  (declare (indent defun))
  `(condition-case-unless-debug e ; soft error handling if loading fails
       (progn ,@body)
     (error (display-warning (make-symbol file)
                             (error-message-string e) :error))))

(let* ((file "startup-config.el")
       (path (file-name-concat neko-root-dir file)))
  (if (file-exists-p path)
      (+safe-progn
        (load path))
    (message "Warning: %s \"%s\" not found" file path)))

(let* ((file "early-config.el")
       (path (file-name-concat neko-personal-dir file)))
  (if (file-exists-p path)
      (+safe-progn
        (load path))
    (message "Warning: %s \"%s\" not found" file path)))
