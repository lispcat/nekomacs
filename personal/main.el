
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

;;; UI

(m/theme)
(m/transparency)
(m/line-numbers)
(m/line-wrap)
(m/mode-line)
(m/scroll)

;;; Programs

(m/term-eat)
(m/magit)
(m/pdf-tools)

;;; Misc

(m/server)


;;; load everything stuff from subdir ./features

(let* ((dir (file-name-concat neko-personal-dir "features"))
       (paths (directory-files-recursively dir "^[^_].*\\.el$")))
  (dolist (path paths)
    (let ((file-as-str path))
      (+load file-as-str))))
