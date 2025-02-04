
(defun mimi/native-transparency-supported? ()
  (if (version<= "29" emacs-version)
      t
    (message "Native transparency is not supported.")
    nil))

(defun mimi/toggle-transparency ()
  (interactive)
  (when (mimi/native-transparency-supported?)
    (let ((alpha (frame-parameter nil 'alpha-background)))
      (set-frame-parameter
       nil 'alpha-background
       (if (eql (cond ((numberp alpha) alpha)
                      ((numberp (cdr alpha)) (cdr alpha))
                      ;; Also handle undocumented (<active> <inactive>) form.
                      ((numberp (cadr alpha)) (cadr alpha)))
		100)
           neko-transparency-value
	 100)))))

(defun mimi/set-transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque: ")
  (when (mimi/native-transparency-supported?)
    (set-frame-parameter (selected-frame) 'alpha-background value)))


(provide 'neko-transparency)
