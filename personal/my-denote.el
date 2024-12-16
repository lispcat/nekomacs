
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
    
    "nl" '(:ignore t :which-key "link")
    "nll" '(denote-find-link :which-key "find links in file")
    "nlf" '(denote-find-link :which-key "find links in file")
    "nln" '(denote-link :which-key "create link")
    "nla" '(denote-add-links :which-key "add links for meta")
    
    "nb" '(:ignore t :which-key "backlink")
    "nbb" '(denote-find-backlink :which-key "find backlinks")
    "nbf" '(denote-find-backlink :which-key "find backlinks")
    "nbl" '(denote-backlinks :which-key "list backlinks")

    "no" '(:ignore t :which-key "org")
    "nol" '(denote-org-extras-dblock-insert-links :which-key "dblock links")
    "nof" '(denote-org-extras-dblock-insert-files :which-key "dblock files")
    "nob" '(denote-org-extras-dblock-insert-backlinks :which-key "dblock backlinks")
    "noa" '(my/denote-insert-file-local-dblock-update-mode
	    :which-key "insert file-local dblock mode")

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

  ;; org-dblocks
  
  (define-minor-mode org-dblock-update-minor-mode
    "A minor mode that automatically updates Org mode dynamic blocks before saving."
    :lighter " OrgDBlocks"
    :global nil
    (if (and org-dblock-update-minor-mode (eq major-mode 'org-mode))
	(add-hook 'before-save-hook #'org-update-all-dblocks nil t)
      (remove-hook 'before-save-hook #'org-update-all-dblocks t)))
  
  (defun my/denote-insert-file-local-dblock-update-mode ()
    (interactive)
    (if (eq major-mode 'org-mode)
	(add-file-local-variable
	 'eval
	 '(org-dblock-update-minor-mode))
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

  ;; dired
  
  (add-hook 'dired-mode-hook #'denote-dired-mode) ; fontify

  ;; functions

  (defun my/denote-directory-jump ()
    (interactive)
    (dired denote-directory))

  ;; capture

  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
		 '("n" "New note (with Denote)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t))))


(use-package consult-notes
  :commands (consult-notes
	     consult-notes-search-in-all-notes)
  :after denote org
  :bind ("M-s n" . consult-notes)
  :general (neko/leader-definer
	     "nf" 'consult-notes
	     "ng" 'consult-notes-search-in-all-notes)
  :config
  (with-eval-after-load 'denote
    (consult-notes-denote-mode 1)))

;; docs: https://lucidmanager.org/productivity/denote-explore/
(use-package denote-explore
  :after denote
  :general
  (neko/leader-definer
    "ne" '(:ignore t :which-key "explore")
    
    ;; random walks
    "new" '(:ignore t :which-key "random walks")
    "newl" '(denote-explore-random-link :which-key "random link")
    "newr" '(denote-explore-random-regex :which-key "random regex")
    "newk" '(denote-explore-random-keyword :which-key "random keyword")

    ;; janitor
    "nej" '(:ignore t :which-key "janitor")
    "nejj" '(denote-explore-sync-metadata :which-key "sync filenames from metadata")
    "nejm" '(denote-explore-sync-metadata :which-key "sync filenames from metadata")
    "nejs" '(denote-explore-sort-keywords :which-key "sort order of all keywords")
    "nejr" '(denote-explore-rename-keyword :which-key "rename keyword")
    "nej0" '(denote-explore-zero-keywords :which-key "0 keywords")
    "nej1" '(denote-explore-single-keywords :which-key "1 keywords")

    ;; visualize
    "nen" '(:ignore t :which-key "network")
    "nenn" '(denote-explore-network :which-key "network")
    "nenr" '(denote-explore-network-regenerate :which-key "network regenerate")
    "nend" '(denote-explore-degree-barchart :which-key "degree barchart")

    ;; stats
    "nes" '(:ignore t :which-key "stats")
    "nesk" '(denote-explore-barchart-keywords :which-key "barchart keywords")
    "nese" '(denote-explore-barchart-filetypes :which-key "barchart filetypes"))
  
  :config
  ;; (setq denote-explore-network-format )
  ;; TODO: make denote-explore-network / browse-url-browser-function
  
  )

;; denote-menu
(use-package denote-menu
  :after denote
  :general
  (neko/leader-definer
    "nm" 'list-denotes)
  :bind
  (:map denote-menu-mode-map
	("c" . denote-menu-clear-filters)
	("/ r" . denote-menu-filter)
	("/ k" . denote-menu-filter-by-keyword)
	("/ o" . denote-menu-filter-out-keyword)
	("e" . denote-menu-export-to-dired)))
