
(use-package server
  :config
  ;; start server at first startup
  (defun ne/start-server-if-not-running ()
    (unless (or (processp server-process)
		(server-running-p))
      (server-start)
      (message "Emacsclient Server started!")))
  (add-hook 'after-init-hook #'ne/start-server-if-not-running))

(neko/leader-definer
  "q" 'delete-frame
  "Q" 'save-buffers-kill-emacs)


(provide 'neko-server)
