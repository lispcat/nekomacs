
;; sudoedit
(use-package auto-sudoedit)

;;; Elfeed ;;;

(use-package elfeed
  :config
  ;; set `elfeed-feeds' to all files in `my/elfeed-feeds-dir'.
  (defvar my/elfeed-feeds-dir "~/feeds")
  (defun my/elfeed-feeds-update-var ()
    (interactive)
    (setq elfeed-feeds
	  (mapcar (lambda (s) (concat "file:" s))
		  (directory-files my/elfeed-feeds-dir t
				   directory-files-no-dot-files-regexp))))
  ;; run `my/elfeed-feeds-update-var' before running `elfeed-update'
  (advice-add #'elfeed-update :before #'my/elfeed-feeds-update-var))
