
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


(provide 'lib-logging-improved)
