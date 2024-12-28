
;;; Productivity ;;;

;;
;; Org-Agenda
;;
;;; The Todo-view:
;;;; / : filter by tag
;;; File:
;;;; C-c C-q : org set tags
;;
;;; Strategies:
;;;; define custom agenda views to quicken
;;;; find all non-tagged tasks, make sure everything is tagged
;;; Todo:
;;;; capture templates


(use-package-local org-agenda
  :after org
  :bind (:map org-agenda-mode-map
	      (")" . 'org-agenda-todo))
  :config
  (defun my/get-org-path (path)
    (expand-file-name path org-directory))
  (setq org-agenda-files
	(list "~/Notes/org/Inbox.org"
	      ;; "~/Notes/org/Personal.org"
	      ;; "~/Notes/org/School.org"
	      ;; "~/Notes/org/Projects.org"
	      ;; "~/Notes/org/Work.org"
	      ))
  (setq org-tag-alist
	'(;; Places
	  ("@home"   . ?H)
	  ("@school" . ?S)
	  ;; ("@work" . ?W)
	  ;; Activities
	  ("@task" . ?t)
	  ("@studying" . ?s)
	  ("@errands"  . ?e)
	  ("@tidy" . ?y)
	  ("@creative" . ?c)
	  ("@art" . ?a)
	  ("@programming" . ?p)
	  ;; ("@calls" . ?l)
	  ;; Devices
	  ("@phone" . ?P)
	  ("@computer" . ?C)))
  
  )

;;; org capture
(use-package-local org-capture
  :after org
  :general
  (neko/leader-definer
    "oc" 'org-capture)
  :config
  (setq org-capture-templates
	(append
	 org-capture-templates
	 `(("t" "Tasks")
	   ("tt" "Task" entry (file ,(my/get-org-path "Inbox.org"))
	    "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)))))
