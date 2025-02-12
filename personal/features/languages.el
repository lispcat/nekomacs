
(defun m/lang-clojure ()
  (use-package clojure-mode
    :defer t))
(m/lang-clojure)

(defun m/lang-scala ()
  (use-package scala-mode
    :interpreter
    ("scala" . scala-mode)
    :hook
    (scala-mode
     . (lambda () (setq prettify-symbols-alist
                   scala-prettify-symbols-alist)))))
(m/lang-scala)

(defun m/lang-zig ()
  (use-package zig-mode
    ;; :config
    ;; (zig-format-on-save-mode 0)
    ))
(m/lang-zig)

(defun m/lang-haskell ()
  (use-package haskell-mode
    :defer t))
(m/lang-haskell)

(use-package cc-mode :local t
  :demand t
  :hook ((c-mode . lsp)
         (c-mode . (lambda () (setq-local lsp-idle-delay 0.1))))
  :config
  (add-to-list 'c-default-style '(c-mode . "cc-mode"))
  (with-eval-after-load 'project
    (define-key c-mode-map (kbd "<f8>")
                (lambda () (interactive)
                  (let ;; universal-argument
                      ((current-prefix-arg '(4)))
                    (call-interactively 'project-compile))))))
