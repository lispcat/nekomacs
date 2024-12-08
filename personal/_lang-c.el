
(defvar ri/c-compile-inputs nil)

;;; NOTE: probably not needed, makefile works.
(defun ri/c-compile-and-run (&optional use-extra)
  "Save the current buffer, compile, and run the C file.
USE-EXTRA will be added to the list of parameters of the run command."
  (interactive)
  (unless (or (eq major-mode 'c-mode) (eq major-mode 'comint-mode) (eq major-mode 'c-ts-mode))
    (error "not in c-mode or comint-mode!"))
  (let* ((main (file-name-nondirectory (buffer-file-name)))
         (exe (file-name-sans-extension main))
         (src (let ((extra-files '("leak_detector_c.c")) ; fill this up
                    (valid-files ""))
                (dolist (p (push main extra-files))
                  (if (file-exists-p
                       (file-name-concat (file-name-directory (buffer-file-name)) p))
                      (setq valid-files (concat valid-files p))))
                valid-files))
         (run-command
          (if (and use-extra ri/c-compile-inputs)
              (concat "gcc " src " -std=gnu11 -lm -g -o " exe " && ./" exe " "
                      ri/c-compile-inputs)
            (concat "gcc " src " -std=gnu11 -lm -g -o " exe " && ./" exe)
            ;; (concat "gcc " src " -std=gnu11 -lm -Wall -Wextra -pedantic -g -o " exe " && valgrind ./" exe " --tool=memcheck --leak-check=full --show-leak-kinds=all --verbose --track-origins=yes")
	    )))
    (save-buffer)
    (compile run-command t)		; -lm for math.h lib
    ;; go through every window in current frame, if match comint-mode, select.
    (let ((orig-win (selected-window))
          (curr-win (next-window)))
      ;; `catch' returns 't if while-loop finds a comint buffer in frame.
      (if (eq 't (catch 'break
                   (let ((inc '0))	; increments per window change
                     (while (not (equal orig-win curr-win))
                       (if (equal 'comint-mode
                                  (with-current-buffer ; returns value of major-mode when
                                      (window-buffer curr-win) ; on buffer of curr-win.
                                    major-mode))
                           (throw 'break t)) ; return t to `catch', found a match
                       ;; select next window
                       (setq curr-win (next-window curr-win))
                       ;; increment by 1 and err if inc value is 1000
                       (setq inc (+ inc 1))
                       (if (equal inc 1000)
                           (progn (user-error "Infinite loop!")
                                  (throw 'break nil))))
                     ;; ^end of while loop
                     (throw 'break nil)))) ; went through all windows, no match.
          (progn
            (select-window curr-win) ; found match, focus on curr-win.
            (end-of-buffer)
            ;; dont make it strongly dedicated!
            (set-window-dedicated-p (selected-window) nil)
            (if (meow-normal-mode-p)
                (meow-insert-mode)))
        (progn
          (message "%s" "could not find compilation buffer!"))))))

(define-key c-mode-map (kbd "<f8>") #'ri/c-compile-and-run)
(define-key c-mode-map (kbd "S-<f8>") (lambda () (interactive)
                                        (ri/c-compile-and-run 't)))
(define-key comint-mode-map (kbd "<f8>") #'quit-window)

;; TODO: if tree-sitter is installed, appropriate keybinds
;; (defun ri/c-treesit-accomidations ()
;;   (setup c-ts-mode
;;     (require 'cc-mode)
;;     ;; TODO: better way? auto choose best lsp?
;;     (:hook lsp)
;;     ;; (define-key c-ts-mode-map (kbd "<f8>") #'ri/c-compile-and-run) 
;;     ;; (define-key c-ts-mode-map (kbd "S-<f8>") (lambda () (interactive)
;;     ;;                                            (ri/c-compile-and-run 't)))
;;     ))
;; (ri/run-func-if-feature-loaded 'treesit #'ri/c-treesit-accomidations)
