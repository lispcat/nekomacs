

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
            ("." ,auto-save-dir t)))))

(provide '50-no-littering)
