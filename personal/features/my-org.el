
;;; Org ;;;

;;; TODO: make bullets use variable font

;; like this?:

;; (let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;                              ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;                              ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;                              ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;                              (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;        (base-font-color     (face-foreground 'default nil 'default))
;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

;;   (custom-theme-set-faces 'user
;;                           `(org-level-8 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-7 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-6 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-5 ((t (,@headline ,@variable-tuple))))
;;                           `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;                           `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
;;                           `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
;;                           `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
;;                           `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil))))))

;; for font-lock-comment-face, INHERIT italic

(setq org-fontify-whole-heading-line t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org :local t
  :config
  (setq org-directory "~/Notes/org")
  (setq org-tags-column -55)
  ;; set org font sizes
  (dolist (pair '((org-document-title :height 1.9 :weight bold)
                  (org-level-1 :height 1.7 :weight bold)
                  (org-level-2 :height 1.4 :weight bold)
                  (org-level-2 :height 1.1)
                  (org-level-3 :height 1.1)))
    (apply #'set-face-attribute (car pair) nil (cdr pair))))

(use-package org-bullets
  :hook org-mode
  :config
  (setq org-bullets-bullet-list
        '("◉"
          "●"
          "○"
          "■"
          "□"
          "✦"
          "✧"
          "✿"
          )))

(use-package org-tempo
  :local t
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("scm" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("unix" . "src conf-unix"))
  (add-to-list 'org-structure-template-alist '("conf" . "src conf"))
  (add-to-list 'org-structure-template-alist '("clang" . "src c"))
  (add-to-list 'org-structure-template-alist '("gcc" . "src c"))
  (add-to-list 'org-structure-template-alist '("java" . "src java")))

(use-package toc-org
  :hook org-mode)

;;; Anki :::

(use-package anki-editor
  :defer t)

;;; Org-download ;;;

(use-package org-download
  :config
  (setq-default org-download-image-dir "_images"))


(setq org-special-ctrl-a/e t)




(defun my/org-get-points-query (query)
  (org-ql-query
    :select #'point-marker
    :from (current-buffer)
    :where query))

(defun my/org-get-points-anki-without-priority ()
  (org-ql-query
    :select #'point-marker
    :from (current-buffer)
    :where '(and (or (property "ANKI_NOTE_ID")
                     (property "ANKI_NOTE_TYPE"))
                 (not (priority)))))

(defun my/org-get-point-summary-root ()
  (car-safe
   (last
    (my/org-get-points-query '(and (heading "Summary") (level 1))))))

(defun my/refile-priority-headings-to-summary ()
  "Refile copy all headings with a priority value to a root-level heading named 'Summary'.
It also cleans up anki headings that don't have a priority value."
  (interactive)
  (when (y-or-n-p "Update subtree of heading 'Summary' with headings with priority?")
    (let ((summary-predicate '(and (heading "Summary") (level 1))))
      (save-excursion
        ;; check anki connection
        (anki-editor-api-check)

        ;; anki-editor-delete-note-at-point for all anki headings without a priority
        (let ((points-anki-but-not-priority
               (my/org-get-points-query '(and (or (property "ANKI_NOTE_ID")
                                                  (property "ANKI_NOTE_TYPE"))
                                              (not (priority))))))
          (dolist (m (reverse points-anki-but-not-priority))
            (goto-char m)
            (when (org-find-property "ANKI_NOTE_ID")
              (anki-editor-delete-note-at-point))
            (when (org-find-property "ANKI_NOTE_TYPE")
              (org-delete-property "ANKI_NOTE_TYPE"))))

        ;; clear summary heading
        (if-let ((target (my/org-get-point-summary-root)))
            (progn
              (goto-char target)
              (forward-line)
              ;; If Summary exists, delete its contents
              (let ((start (point)))
                (org-end-of-subtree)
                (delete-region start (point))))
          ;; If Summary doesn't exist, create it
          (goto-char (point-max))
          (insert "* Summary"))

        ;; set all priority headings to use anki default note type
        (dolist (m (reverse (my/org-get-points-query '(priority))))
          (goto-char m)
          (anki-editor-set-note-type nil "Basic"))

        ;; refile copy each heading to summary
        ;; no need to reverse list here
        (dolist (m (my/org-get-points-query '(priority)))
          (goto-char m)
          (let ((org-refile-keep t)
                (rfloc (list "Summary" (buffer-file-name) nil
                             (my/org-get-point-summary-root))))
            (org-refile nil nil rfloc "Copy")
            (set-marker m nil)))

        ;; delete anki properties below summary-pos
        (let ((summary-marker (my/org-get-point-summary-root)))
          (delete-matching-lines ":ANKI_NOTE_TYPE:" summary-marker (point-max))
          (delete-matching-lines ":ANKI_NOTE_ID:" summary-marker (point-max)))

        ;; done
        (message "done")))))


;;; latex setup

(use-package auctex)

(use-package cdlatex
  :hook (org-mode . turn-on-org-cdlatex)
  :config
  (defun org-try-cdlatex-tab ()
    "Check if it makes sense to execute `cdlatex-tab', and do it if yes.
It makes sense to do so if `org-cdlatex-mode' is active and if the cursor is
  - inside a LaTeX fragment, or
  - after the first word in a line, where an abbreviation expansion could
    insert a LaTeX environment."
    (when org-cdlatex-mode
      (cond
       ;; Before any word on the line: No expansion possible.
       ;; ((save-excursion (skip-chars-backward " \t") (bolp)) nil)
       ;; Just after first word on the line: Expand it.  Make sure it
       ;; cannot happen on headlines, though.
       ;; ((save-excursion
       ;;    (skip-chars-backward "a-zA-Z0-9*")
       ;;    (skip-chars-backward " \t")
       ;;    (and (bolp) (not (org-at-heading-p))))
       ;;  (cdlatex-tab) t)
       ((org-inside-LaTeX-fragment-p) (cdlatex-tab) t)))))

(use-package org-fragtog
  :hook org-mode)






;; ;; First, ensure AUCTeX completions are available
;; (require 'tex-mode)
;; (require 'latex)

;; ;; Define a function to check if we're in a LaTeX fragment
;; (defun my/org-in-latex-fragment-p ()
;;   "Return non-nil if point is in a LaTeX fragment or environment."
;;   (or (org-inside-LaTeX-fragment-p)
;;       (org-inside-latex-env-p)))

;; ;; Create a completion function that uses AUCTeX's completion
;; (defun my/auctex-completions ()
;;   "Get completions from AUCTeX when in LaTeX context."
;;   (when (my/org-in-latex-fragment-p)
;;     (let ((comp (TeX-completion-symbol)))
;;       (when comp
;;         (list :company-candidates comp)))))

;; ;; Add the completion function to Cape
;; (setq cape-tex-commands
;;       (mapcar (lambda (cmd) (concat "\\" cmd))
;;               (append TeX-symbol-list LaTeX-math-list)))

;; (use-package auctext-c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; USE AUCTEX COMPANY AND WRAP WITH CAPE?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; ;; Set up the completion sources
;; (defun my/setup-latex-completion ()
;;   (setq-local completion-at-point-functions
;;               (list (cape-super-capf

;;                      #'cape-tex
;;                      #'cape-dict)))
;;   ;; Optional: Configure Corfu for immediate activation in LaTeX contexts
;;   (setq-local corfu-auto t
;;            corfu-auto-delay 0.2
;;            corfu-auto-prefix 2))

;; ;; ;; Add to org-mode-hook
;; (add-hook 'org-mode-hook #'my/setup-latex-completion)




;;;;; FIXES IMAGE JUMP SCROLLING
;; (use-package org-sliced-images
;;   :ensure t
;;   :config (org-sliced-images-mode))



;;; JUST ONE PROBLEM: meow mode cursor doesnt work.

(require 'image-slicing)
(setq image-slicing-newline-trailing-text nil)
(add-hook 'org-mode-hook #'image-slicing-mode)
;; (remove-hook 'org-mode-hook #'image-slicing-mode)





(use-package orglink
  :hook prog-mode)
