
;;; Productivity ;;;

;;
;; Org-Agenda
;;
;; The Todo-view
;;   /  filter by tag
;; File
;;   C-c C-q  org set tags
;;
;; Strategies
;; - define custom agenda views to quicken
;; - find all non-tagged tasks, make sure everything is tagged
;; Todo
;; - capture templates


;;;;;; Agenda ;;;;;;

(use-package org-agenda
  :local t
  :after org
  :bind (:map org-agenda-mode-map
              (")" . 'org-agenda-todo))
  :config
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "PROG(p)"
                    "|"
                    "DONE(d/!)")))
  (setq org-agenda-files
        (list "~/Notes/org/Inbox.org"
              "~/Notes/denote/20250131T044005--agenda__agenda.org"))
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
          ("@today" . ?T)
          ;; ("@calls" . ?l)
          ;; Devices
          ("@phone" . ?P)
          ("@computer" . ?C)))
  (defun my/org-get-prop-time ()
    (if (not (eq major-mode 'org-mode)) ""
      (let ((val (org-entry-get nil "TIME")))
        (if (not val) ""
          (format "%s" (string-trim val))))))
  (defun my/org-get-prop-effort ()
    (if (not (eq major-mode 'org-mode)) ""
      (let ((val (org-entry-get nil "EFFORT")))
        (if (not val) ""
          (format "%s" (string-trim val))))))
  (defun my/org-get-prop-days ()
    (if (not (eq major-mode 'org-mode)) ""
      (let ((val (org-entry-get nil "DAYS")))
        (if (not val) ""
          (format "%s" (string-trim val))))))
  (setq org-agenda-prefix-format
        `((agenda
           . ,(concat " %i "
                      "%?-12t"
                      "[%3(my/org-get-prop-effort)]   "
                      ;; "%3(my/org-get-prop-effort)  "
                      "% s"))
          (todo   . " %i ")
          (tags   . " %i %-12:c")
          ;; (search . " %i %-12:c")
          (search . " %c")
          ))
  ;; (setq org-agenda-sorting-strategy
  ;;       ((agenda habit-down time-up urgency-down category-keep user-defined-up)
  ;;        (todo urgency-down category-keep)
  ;;        (tags urgency-down category-keep)
  ;;        (search category-keep)))
  )

(->>
 (progn

   (defun my/org-get-ts (prop)
     (when (eq major-mode 'org-mode)
       (org-entry-get nil prop)))

   (defun my/org-get-repeater-cookie (ts)
     (when-let ((regex  org-ql-regexp-part-ts-repeaters)
                (_match (string-match regex ts))
                (cookie (match-string 0 ts)))
       (string-trim cookie)))

   (defun my/org-get-pre-repeater-cookie (ts)
     (when-let ((regex  (concat "\\(.*\\)"
                                org-ql-regexp-part-ts-repeaters))
                (_match (string-match regex ts))
                (cookie (match-string 1 ts)))
       (string-trim cookie)))

   (defun my/org--match-regex-string (str regex num)
     (when (string-match regex str)
       (string-trim (match-string num str))))

   ;; (defun my/org-set-repeater-cookie (prop new-cookie)
   ;;   (interactive
   ;;    (list
   ;;     (read-string "Name of timestamp property?: ")
   ;;     (read-string "Cookie value to set?: ")))
   ;;   (when-let ((_org? (eq major-mode 'org-mode))
   ;;              (ts (org-entry-get nil prop)))
   ;;     (if-let ((pre-cookie (my/org--match-regex-string
   ;;                           ts
   ;;                           (concat "\\(.*\\)"
   ;;                                   org-ql-regexp-part-ts-repeaters)
   ;;                           1))
   ;;              (old-cookie (my/org--match-regex-string
   ;;                           ts
   ;;                           org-ql-regexp-part-ts-repeaters
   ;;                           0))
   ;;              (new-ts (concat pre-cookie " " new-cookie ">")))
   ;;         (message "NEW1: %s" new-ts)
   ;;         (org-entry-put nil prop new-ts)
   ;;       (when-let ((ts-no-arrow
   ;;                   (my/org--match-regex-string
   ;;                    ts
   ;;                    "\\(?:.*\\)>"
   ;;                    0))
   ;;                  (ts-new (concat ts-no-arrow " " new-cookie)))
   ;;         (message "NEW2: %s" ts-new)
   ;;         (org-entry-put nil prop ts-new)))))
   )

 (with-eval-after-load 'org-ql)
 (with-eval-after-load 'org))

(use-package org-super-agenda
  :after org org-agenda
  :config
  (org-super-agenda-mode 1)
  (setq org-agenda-custom-commands
        `(
          ("a" "main agenda"
           ((todo "PROG|NEXT")
            (agenda
             ""
             ((org-agenda-show-future-repeats nil)
              (org-agenda-start-on-weekday 1)
              ;; (org-super-agenda-groups
              ;;  '((:anything t)))
              ))))
          )))

(defun my/org-clone-with-fraction (days time effort)
  "Clone subtree with time shifts, prefixing each subheading with fraction prefix."
  (interactive
   (list
    (read-number "How many days to complete it over?: ")
    (read-number "How many minutes do you expect this task to take?: ")
    (read-number "On a scale of 1-10, how much effort will this take?: ")))
  (setq days (1- days))
  ;; create clones
  (org-clone-subtree-with-time-shift days "-1d")
  (org-set-property "TIME" (format "%s" time))
  (org-set-property "EFFORT" (format "%s" effort))
  ;; adjust appropriately
  (save-excursion
    (org-next-visible-heading 1)
    ;; first, sort
    (cl-loop for depth from (1- days) downto 1 do
             (save-excursion
               ;; shift
               (dotimes (_ depth)
                 (org-metadown))))
    ;; add todo and demote
    (save-excursion
      (cl-loop repeat (1- days) do
               (org-next-visible-heading 1))
      (cl-loop for depth from (1- days) downto 0 do
               (let ((frac (format "%d/%d" (1+ depth) days))
                     (time-daily (/ time days)))
                 (org-demote)
                 (let ((org-special-ctrl-a/e t))
                   (org-beginning-of-line))
                 (insert (concat frac " "))
                 (org-set-property "FRACTION" frac)
                 (org-set-property "TIME" (format "%s" time-daily))
                 (org-set-property "EFFORT" (format "%s" effort))
                 (org-next-visible-heading -1))))))

;; TODO: while in an org-agenda view, function "my/do-a-task", org pomo start on the asgn with the lowest effort value.

;;; TODO: Linear programming solver to show tasks to do for the day that would achieve the minimum amount of work i need to do for the given day over the next 7 days worth of assignments. have it consider effort and time properties.

;;;;;; Org Capture ;;;;;;

(use-package org-capture
  :local t
  :after org
  :general
  (neko/leader-definer
    "oc" 'org-capture)
  :config
  (defun my/get-org-agenda-denote-file (name)
    (let ((regex (format "^.*--%s__.*\\.org$" name)))
      (car (seq-filter
            (lambda (path)
              (string-match regex (file-name-nondirectory path)))
            org-agenda-files))))
  (setq org-capture-templates
        `(("t" "Tasks")
          ("td" "Todo with deadline" entry
           (file ,(my/get-org-agenda-denote-file "agenda"))
           "* TODO %^{Task}\nDEADLINE: %^{Deadline}t\n%?\n"
           :empty-lines 1
           :immediate-finish nil)
          ("tp" "Task" entry
           (file ,(my/get-org-agenda-denote-file "agenda"))
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
          ("n" "New note (with Denote)" plain
           (file denote-last-path)
           #'denote-org-capture :no-save t :immediate-finish nil
           :kill-buffer t :jump-to-captured t))))

;;;;;; Org QL ;;;;;;

(use-package org-ql)

;;;;;; org pomodoro ;;;;;;

(use-package org-pomodoro)
