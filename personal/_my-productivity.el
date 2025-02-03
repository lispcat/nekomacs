
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
    (append org-agenda-files (cl-remove-duplicates (my/org-agenda-denote-all-todo my-classes-dir)))))

(defun my/always-update-and-get-org-agenda-files ()
  (let ((my-classes-dir "~/Notes/denote/classes/"))
    (my/update-denote-keywords-todo my-classes-dir)
    (append org-agenda-files (my/org-agenda-denote-all-todo my-classes-dir))))

(with-eval-after-load 'org-ql
  (with-eval-after-load 'denote
    (setq
     org-agenda-custom-commands
     '(;; Agenda
       ("r" "MyView"
	((agenda "" ((org-agenda-span 2)
                     (org-super-agenda-groups
                      '((:name "Today"
                               :time-grid t
                               :date today
                               :todo "TODAY"
                               :scheduled today
                               :order 1)
                        (:discard (:anything t))
                        ))))
	 (alltodo "" ((org-agenda-overriding )))))
       ("a" "All agenda" agenda	""
	((org-agenda-files (my/maybe-update-and-get-org-agenda-files))
	 (org-agenda-overriding-header "All agenda")))
       
       ;; Agenda + Refresh
       ("A" "All agenda + Refresh" agenda ""
	((org-agenda-files (my/always-update-and-get-org-agenda-files))
	 (org-agenda-overriding-header "All agenda + Refresh")))

       ;; agenda-like todo view
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

       ;; fixed?
       ("o" "new"
	((org-ql-block
	  '(or (and (not (done))
		    (or (deadline :to today)
			(scheduled :to today)
			(ts-active :to today)))
	       (closed :on today))
	  ((org-ql-block-header "new: Due today")
	   (org-agenda-files
	    '("~/Notes/denote/classes/20241215T083836--spring-classes__class_meta_todo.org")))
	  :sort '(todo priority date))))

       ;; daily due
       ("d" "day"
        ((org-ql-block '(and (not (done))
                             (or (deadline :to -1)
                                 (scheduled :to -1)
                                 (ts-active :to -1)))
                       ((org-ql-block-header "Due yesterday")
                        (org-agenda-files (my/maybe-update-and-get-org-agenda-files)))
                       :sort '(todo priority date))
         (org-ql-block '(or (and (not (done))
                                 (or (deadline :to today)
                                     (scheduled :to today)
                                     (ts-active :to today)))
                            (closed :on today))
                       ((org-ql-block-header "Due today")
                        (org-agenda-files (my/maybe-update-and-get-org-agenda-files)))
                       :sort '(todo priority date))
         (org-ql-block '(and (not (done))
                             (or (deadline :on +1)
                                 (scheduled :on +1)
                                 (ts-active :on +1)))
                       ((org-ql-block-header "Due tomorrow")
                        (org-agenda-files (my/maybe-update-and-get-org-agenda-files)))
                       :sort '(todo priority date))
         (org-ql-block '(and (not (done))
                             (or (deadline :from +2 :to +7)
                                 (scheduled :from +2 :to +7)
                                 (ts-active :from +2 :to +7)))
                       ((org-ql-block-header "Due this week")
                        (org-agenda-files (my/maybe-update-and-get-org-agenda-files)))
                       :sort '(priority date))
         (org-ql-block '(and (not (done))
                             (or (deadline :from +7 :to +31)
                                 (scheduled :from +7 :to +31)
                                 (ts-active :from +7 :to +31)))
                       ((org-ql-block-header "Due later")
                        (org-agenda-files (my/maybe-update-and-get-org-agenda-files)))
                       :sort '(priority date))))))))

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
               (effort (org-entry-get (plist-get properties :org-marker) "Effort"))
	       (progress (org-entry-get (plist-get properties :org-marker) "Progress")))
	  (org-add-props
              (format " [%3s] %3s%s" (concat effort) (concat progress) result)
              (text-properties-at 0 result)))))
    (advice-add 'org-ql-view--format-element :around #'zdo/org-ql-view--format-element)))

;;; progress property

(defun my/org-increment-progress-property ()
  "Increment the PROGRESS property to its next fraction value."
  (interactive)
  (when-let* ((current-value (org-entry-get nil "PROGRESS"))
              (parts (when current-value (split-string current-value "/"))))
    (when parts
      (let* ((current-numerator (string-to-number (car parts)))
             (denominator (string-to-number (cadr parts)))
             (next-numerator (1+ current-numerator)))
	(when (<= next-numerator denominator)
	  (org-entry-put nil "PROGRESS" (format "%d/%d" next-numerator denominator)))
	(when (or (>= next-numerator denominator))
	  (message "Task fully completed"))))))

(defun my/org-todo-increment-progress-property ()
  (interactive)
  (when (org-entry-is-done-p)
    (my/org-increment-progress-property)))

(add-hook 'org-after-todo-state-change-hook 'my/org-todo-increment-progress-property)


;;;; TODO:
;;;; - org-ql to get all headings in the current file with TODO (for bio and notes)
