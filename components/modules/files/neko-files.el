
;;; Functions:

(defun mimi/open-nekomimi-personal-dir ()
  (interactive)
  (dired nekomimi-personal-dir))

;;; Set leader-key binds:

(mimi/leader-define-key
 "f" '(:ignore t :which-key "files")
 "ff" 'find-file
 "fp" 'mimi/open-nekomimi-personal-dir)


(provide 'neko-files)
