
;; Sample config:
;; https://protesilaos.com/emacs/denote#h:5d16932d-4f7b-493d-8e6a-e5c396b15fd6

;; Note:
;; - dired: "% m" then "t" then `k' to kill and filter down results

(use-package denote
  :demand t
  :general ; this defers
  (neko/leader-definer
    "n" '(:ignore t :which-key "denote")
    "nn" 'denote
    "ns" 'denote-subdirectory
    
    "nf" 'denote-open-or-create

    "nd" '(:ignore t :which-key "directory")
    "ndj" '(my/denote-directory-jump :which-key "jump to denote dir")
    
    "ni" '(:ignore t :which-key "silo")
    "nid" '(my/denote-silo-extras-dired-to-silo :which-key "dired")
    "nic" '(denote-silo-extras-select-silo-then-command :which-key "command")
    "nin" '(denote-silo-extras-create-note :which-key "create")
    "nio" '(denote-silo-extras-open-or-create :which-key "open")

    "nl" '(:ignore t :which-key "link")
    "nll" '(denote-link :which-key "create link") 
    "nlf" '(denote-find-link :which-key "find links in file")
    "nla" '(denote-add-links :which-key "add links for meta")
    
    "nb" '(:ignore t :which-key "backlink")
    "nbb" '(denote-backlinks :which-key "show backlinks")
    "nbf" '(denote-find-backlink :which-key "find backlinks")

    "no" '(:ignore t :which-key "org")
    "nol" '(denote-org-extras-dblock-insert-links :which-key "dblock links")
    "nof" '(denote-org-extras-dblock-insert-files :which-key "dblock files")
    "nob" '(denote-org-extras-dblock-insert-backlinks :which-key "dblock backlinks")
    "noa" '(my/denote-insert-file-local-dblock-auto-update :which-key "insert file-local dblock auto-update")

    "nj" '(:ignore t :which-key "journal")
    "njn" '(denote-journal-extras-new-entry :which-key "new entry")
    "njl" '(denote-journal-extras-link-or-create-entry :which-key "link entry")
    "njo" '(denote-journal-extras-new-or-existing-entry :which-key "open today")
    "njj" '(denote-journal-extras-new-or-existing-entry :which-key "open today"))
  
  :config

  (setq denote-directory (expand-file-name "~/Notes/denote"))
  (setq denote-known-keywords '("emacs" "class" "ideas" "art"
				"hobbies" "random"))
  (setq denote-prompts '(title keywords subdirectory))
  
  ;; exclude any subdir: [ Archive[d]?, Exclude[d]?, _.* ] where "(^|/).*(/|$)"
  (setq denote-excluded-directories-regexp
	(concat
	 "\\(^\\|/\\)" "[aA]rchived?" "\\(/\\|$\\)" "\\|"
	 "\\(^\\|/\\)" "[eE]xcluded?" "\\(/\\|$\\)" "\\|"
	 "\\(^\\|/\\)" "_.*"          "\\(^\\|/\\)"
	 ))

  ;; misc settings
  (setq denote-rename-confirmations '(rewrite-front-matter))

  ;; dblocks
  (defun my/denote-insert-file-local-dblock-auto-update ()
    (interactive)
    (if (eq major-mode 'org-mode)
	(add-file-local-variable
	 'eval
	 '(add-hook 'before-save-hook #'org-update-all-dblocks nil t))
      (message "Not in an org-mode buffer")))

  ;; rename buffer
  
  (setq denote-rename-buffer-format "[D] %t%b  _%k")
  (denote-rename-buffer-mode 1)

  ;; text files
  
  (add-hook 'text-mode-hook #'denote-fontify-links-mode-maybe)

  ;; journal

  (require 'denote-journal-extras)
  (setq denote-journal-extras-directory
	(expand-file-name "journal" denote-directory))

  ;; dired fontify
  (add-hook 'dired-mode-hook #'denote-dired-mode)

  ;; silos
  
  (require 'denote-silo-extras)
  
  ;; (defvar my/denote-school-silo
  ;;   (expand-file-name "~/School/classes/denote"))
  
  ;; (setq denote-silo-extras-directories
  ;; 	(list denote-directory
  ;; 	      my/denote-school-silo))

  ;; silo functions

  (defun my/denote-directory-jump ()
    (interactive)
    (dired denote-directory))

  (defun my/denote-silo-extras-dired-to-silo (silo)
    "Switch to SILO directory using `dired'.
SILO is a file path from `denote-silo-extras-directories'.

When called from Lisp, SILO is a file system path to a directory that
conforms with `denote-silo-extras-path-is-silo-p'."
    (interactive (list (denote-silo-extras-directory-prompt)))
    (denote-silo-extras-with-silo silo
      (dired silo)))

  (defun my/denote-silo-extras-cd-to-silo (silo)
    "Switch to SILO directory using `cd'.
SILO is a file path from `denote-silo-extras-directories'.

When called from Lisp, SILO is a file system path to a directory that
conforms with `denote-silo-extras-path-is-silo-p'."
    (interactive (list (denote-silo-extras-directory-prompt)))
    (denote-silo-extras-with-silo silo
      (cd silo)))

  ;; capture

  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
		 '("n" "New note (with Denote)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))

  ;; which-key

  )
