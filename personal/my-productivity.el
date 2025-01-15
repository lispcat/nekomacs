
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
	      ;; "~/Notes/denote/classes/20250106T171222--agenda__agenda.org"
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

;;; org-ql

(use-package org-ql
  :preface (straight-register-package '(org :type built-in)))

;;; denote - todo - parser:

(defun my/org-file-has-active-todos-or-timestamps-p (file)
  "Check if FILE has any non-DONE TODOs or any timestamps.
Returns t if found, nil otherwise."
  (let ((buf (find-file-noselect file)))
    (prog1
	(with-current-buffer (find-file-noselect file)
	  (org-ql-select (current-buffer)
	    '(or (todo)		       ; any TODO state except DONE
		 (ts :from -1))	       ; any timestamp, past or future
	    :action '(lambda () t)	       ; return t if any match found
	    :narrow t))
      (kill-buffer buf))))

(defun my/process-org-files (directory)
  "Process org files in DIRECTORY to manage TODO keyword consistency.
Returns a list of two lists: (files-needing-todo files-removing-todo)."
  (let* ((matching-files '())
         (non-matching-files '())
         (all-files
	  (seq-filter (lambda (path)
			(let ((basename (file-name-nondirectory path)))
			  (not (or (string-prefix-p ".#" basename)
				   (auto-save-file-name-p basename)
				   (backup-file-name-p basename)))))
		      (directory-files-recursively
		       directory "\\.org$" nil
		       (lambda (dir)
			 (not (string-match-p denote-excluded-directories-regexp dir)))))))
    
    ;; Sort files into matching and non-matching
    (dolist (file all-files)
      (if (my/org-file-has-active-todos-or-timestamps-p file)
          (push file matching-files)
        (push file non-matching-files)))
    
    ;; Process each list to ensure keyword consistency
    (let ((files-needing-todo '())
          (files-removing-todo '()))
      
      ;; Check matching files - should have 'todo' keyword
      (dolist (file matching-files)
        (unless (member "todo" (denote-extract-keywords-from-path file))
          (push file files-needing-todo)))
      
      ;; Check non-matching files - should not have 'todo' keyword
      (dolist (file non-matching-files)
        (when (member "todo" (denote-extract-keywords-from-path file))
          (push file files-removing-todo)))
      
      (list files-needing-todo files-removing-todo))))

(defun my/update-denote-keywords-todo (directory)
  "Update denote keywords in DIRECTORY based on TODO/timestamp status.
Add 'todo' keyword to files with TODOs/timestamps, remove from those without."
  (interactive "DDirectory: ")
  (let* ((results (my/process-org-files directory))
         (files-needing-todo (car results))
         (files-removing-todo (cadr results)))
    (if (not (or files-needing-todo files-removing-todo))
	(message "No files to change")
      (when (yes-or-no-p (format "Continue?\n- Add todo: %s\n- Remove todo: %s"
				 files-needing-todo files-removing-todo))
	;; Add 'todo' keyword where needed
	(dolist (file files-needing-todo)
	  ;; denote add keyword
	  (denote-rename-file file 'keep-current
			      (cons "todo" (denote-extract-keywords-from-path file))
			      'keep-current 'keep-current))
	;; Remove 'todo' keyword where not needed
	(dolist (file files-removing-todo)
	  ;; denote add keyword
	  (denote-rename-file file 'keep-current
			      (remove "todo" (denote-extract-keywords-from-path file))
			      'keep-current 'keep-current))
	;; Report results
	(message "Added 'todo' keyword to %d files, removed from %d files"
		 (length files-needing-todo)
		 (length files-removing-todo))))))

(defvar my/org-agenda-scanner-last-ran -1)

(defun my/maybe-update-denote-keywords-todo (directory)
  (when-let ((cur (string-to-number (emacs-uptime "%h")))
	     (last my/org-agenda-scanner-last-ran)
	     (since (- cur last))
	     (pred (or (= last -1) (> since 12))))
    (setq my/org-agenda-scanner-last-ran cur)
    (my/update-denote-keywords-todo directory)))

;;; org-agenda setup

(defun my/denote-trim-identifier-current ()
  (let ((file (buffer-file-name)))
    (if (not file) ""
      (let ((file (file-name-nondirectory file)))
	(string-match "^.*--\\(.*\\)__.*\\.org$" file)
	(concat (match-string 1 file) ": ")))))

(with-eval-after-load 'org-agenda
  (setq org-agenda-prefix-format
	'((agenda . " %i %-12(my/denote-trim-identifier-current)%?-12t% s")
	  (todo   . " %i %-12(my/denote-trim-identifier-current)")
	  (tags   . " %i [%-3e] %-12(my/denote-trim-identifier-current)")
	  (search . " %i %-12(my/denote-trim-identifier-current)"))))

(defun my/org-agenda-denote-all-todo (search-path)
  (seq-filter (lambda (path)
		(let ((basename (file-name-nondirectory path)))
		  (not (or (string-prefix-p ".#" basename)
			   (auto-save-file-name-p basename)
			   (backup-file-name-p basename)))))
	      (directory-files-recursively
	       search-path "_todo.*\\.org$" nil
	       (lambda (dir) (not (string-match-p denote-excluded-directories-regexp dir))))))

(defun my/maybe-update-and-get-org-agenda-files ()
  (let ((my-classes-dir "~/Notes/denote/classes/"))
    (my/maybe-update-denote-keywords-todo my-classes-dir)
    (append org-agenda-files (my/org-agenda-denote-all-todo my-classes-dir))))

(defun my/always-update-and-get-org-agenda-files ()
  (let ((my-classes-dir "~/Notes/denote/classes/"))
    (my/update-denote-keywords-todo my-classes-dir)
    (append org-agenda-files (my/org-agenda-denote-all-todo my-classes-dir))))

(with-eval-after-load 'org-ql
  (with-eval-after-load 'denote
    (setq
     org-agenda-custom-commands
     '(;; Agenda
       ("a" "All agenda" agenda	""
	((org-agenda-files (my/maybe-update-and-get-org-agenda-files))
	 (org-agenda-overriding-header "All agenda")))
       
       ;; Agenda + Refresh
       ("A" "All agenda + Refresh" agenda ""
	((org-agenda-files (my/always-update-and-get-org-agenda-files))
	 (org-agenda-overriding-header "All agenda + Refresh")))

       ;; todo view
       ("t" "todo"
	((org-ql-block
	  '(or (and (not (done))
		    (or (habit)
			(deadline auto)
			(scheduled :to today)
			(ts-active :on today)))
	       (closed :on today))
	  ((org-ql-block-header "Due today")
	   (org-agenda-files (my/maybe-update-and-get-org-agenda-files)))
	  :sort '(todo priority date))))

       ;; daily due
       ("d" "today"
	((org-ql-block '(and (todo)
			     (deadline :on today))
		       ((org-ql-block-header "Due today")
			(org-agenda-files (my/maybe-update-and-get-org-agenda-files))))
	 (org-ql-block '(and (todo)
			     (deadline :on +1))
		       ((org-ql-block-header "Due tomorrow")
			(org-agenda-files (my/maybe-update-and-get-org-agenda-files))))
	 (org-ql-block '(and (todo)
			     (or
			      (deadline :on +2)
			      (deadline :on +3)
			      (deadline :on +4)))
		       ((org-ql-block-header "Due later")
			(org-agenda-files (my/maybe-update-and-get-org-agenda-files))))))
       
       ))))

;; show effort
(with-eval-after-load 'org-ql
  (with-eval-after-load 'denote
    (defun zdo/org-ql-view--format-element (orig-fun &rest args)
      "This function will intercept the original function and
   add the category to the result.

   ARGS is `element' in `org-ql-view--format-element'"
      (if (not args)
	  ""
	(let* ((element args)
               (properties (cadar element))
               (result (apply orig-fun element))
               (category (org-entry-get (plist-get properties :org-marker) "Effort")))
	  (org-add-props
              (format " [%3s]%s" (concat category) result)
              (text-properties-at 0 result)))))
    (advice-add 'org-ql-view--format-element :around #'zdo/org-ql-view--format-element)))
  

;; (defun zdo/org-ql-view--format-element (orig-fun &rest args)
;;   "This function will intercept the original function and
;; add the category to the result.

;; ARGS is `element' in `org-ql-view--format-element'"
;;   (if (not args)
;;       ""
;;     (let* ((element args)
;;            (properties (cadar element))
;;            (result (apply orig-fun element))
;;            (smt "")
;;            (category (org-entry-get (plist-get properties :org-marker) "Effort")))
;;       (if (> (length category) 11)
;;           (setq category (substring category 0 10)))
;;       (if (< (length category) 11)
;;           (setq smt (make-string (- 11 (length category)) ?\s)))
;;       (org-add-props
;; 	  (format "  %-3s %s" (concat category ":" smt) result)
;; 	  (text-properties-at 0 result)))))
;; (advice-add 'org-ql-view--format-element :around #'zdo/org-ql-view--format-element)
