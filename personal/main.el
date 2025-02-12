
(message "DEBUG: enter main.el")

;;; Emacs

(m/essential-tweaks)
(m/dired)
(m/buffers)
(m/files)
(m/history)
(m/windows)
(m/helpful)
(m/auto-sudoedit)

;;; Completion

(m/vertico)
(m/cape)
(m/consult)
(m/corfu)
(m/embark)
(m/embark-consult)
(m/marginalia)
(m/orderless)
(m/yasnippet)

;;; IDE

(m/ide-essentials)

(m/lsp-mode)
(m/eglot)

(m/lang-essentials)
(m/lang-elisp)
(m/lang-lisp-advanced)
(m/lang-markdown)
(m/lang-rust)
(m/lang-scheme)

;;; Org

(m/org)
(m/org-agenda)

;;; Keyboard

(m/meow)
;; (m/meow-qwerty)
(m/meow-dvp)
(m/avy)
(m/spellcheck)

;;; UI

(m/theme)
(m/transparency)
(m/line-numbers)
(m/line-wrap)
(m/mode-line-tweaks)
(m/doom-modeline)
(m/scroll)

;;; Programs

(m/term-eat)
(m/magit)
(m/pdf-tools)

;;; Misc

(m/server)

;;; add stuff in vendor dir to load-path
(let ((vendor-dir (file-name-concat neko-personal-dir "vendor/")))
  (dolist (path (directory-files vendor-dir t))
    (when (file-directory-p path)
      (add-to-list 'load-path path))))

;;; load everything stuff from subdir ./features

(let* ((dir (file-name-concat neko-personal-dir "features"))
       (paths (directory-files-recursively dir "^[^_].*\\.el$")))
  (dolist (path paths)
    (let ((file-as-str path))
      (+safe-progn
        (load file-as-str)))))
