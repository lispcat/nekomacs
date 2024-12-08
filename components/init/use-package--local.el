
;;; TODO: BROKEN

(require 'use-package-core) ;; does this suffice?


(defalias 'use-package-normalize/:local 'use-package-normalize-predicate)

(defun use-package-handler/:local (name keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (if arg
	(let* ((extra-keywords '(:ensure nil :straight nil))
	       (state (plist-put state :ensure nil))
	       (state (plist-put state :straight nil))
	       )
	  (message "DEBUG: Name: %s" name)
	  (message "DEBUG: Rest: %s" (append extra-keywords rest))
	  (message "DEBUG: State: %s" state)
	  (use-package-process-keywords
	    name
	    (append extra-keywords rest)
	    state
	    ))
      body)))

(add-to-list 'use-package-keywords :local)


(provide 'use-package--local)

