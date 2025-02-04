
;;; Line numbers

;; list of programming modes to disable line-numbers on
(defvar neko/display-line-numbers-exclude '())

;; enable line-numbers on programming modes
(add-hook 'prog-mode-hook
	  (lambda ()
	    (unless (memq major-mode neko/display-line-numbers-exclude)
	      (display-line-numbers-mode 1))))

;;; Line wrap

(global-visual-line-mode 1) 
(diminish 'visual-line-mode) ; dont show minor-mode indicator "Wrap" in mode-line


(provide 'neko-line)
