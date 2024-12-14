
;; https://protesilaos.com/emacs/denote#h:5d16932d-4f7b-493d-8e6a-e5c396b15fd6
(use-package denote
  :config
  (setq denote-directory (expand-file-name "~/Notes/denote"))
  (setq denote-known-keywords '("emacs" "class" "ideas" "art" "hobbies" "random"))
  ;; (setq denote-prompts '(title keywords subdirectory)) ; cannot interact with rest

  ;; silos
  (require 'denote-silo-extras)
  (setq denote-silo-extras-directories
	(list denote-directory
	      (expand-file-name "~/School/classes/denote")))
  

  (mimi/leader-define-key
    "n" '(:ignore t :which-key t)
    "nn" 'denote
    "nl" 'denote-link))
