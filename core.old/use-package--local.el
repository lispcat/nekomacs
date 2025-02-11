(require 'use-package-core) ;; does this suffice?


;;;;;; Variables ;;;;;;

(defvar use-package-universal-keywords-alist
  '((:ensure   . (featurep 'package))
    (:straight . (featurep 'straight))
    (:elpaca   . (featurep 'elpaca)))
  "Alist of (keyword . predicate) pairs.
The keyword will be included in :local expansion if predicate evaluates to non-nil.")

(defvar use-package-universal-fetch-keyword
  :straight)

;;;;;; Functions ;;;;;;

(defun use-package-universal--working-kwds ()
  "Return a list of available `use-package' keywords.
This is achieved by evaluating each predicate from
`use-package-universal-keywords-alist' and returning each corresponding keyword
if it evals non-nil."
  (apply #'append
         (mapcar (lambda (pair)
                   (when (eval (cdr pair))
                     (list (car pair))))
                 use-package-universal-keywords-alist)))

(defun use-package-universal--filter-kwds-and-val (rest keywords)
  "Filter every keyword from KEYWORDS and its corresponding value from REST.
REST should be from a use-package-handler argument, containing a list of
keywords and its values.
KEYWORDS should be a list of keywords to filter out from REST.
For every matching keyword, its following element (value) is discarded."
  (cl-loop for (k v) on rest by #'cddr
           unless (memq k keywords)
           nconc (list k v)))

(defun use-package-universal--ins-nil-after-each-kwd (keywords)
  "Return a list with nil after every keyword in KEYWORDS."
  (apply #'append
         (mapcar (lambda (key)
                   (list key 'nil))
                 keywords)))

;;; :local ;;;

(defalias 'use-package-normalize/:local 'use-package-normalize-predicate)

(defun use-package-handler/:local (name keyword arg rest state)
  "Use-package handler for :local.
NAME: name of package.
KEYWORD: the \":local\" keyword.
ARG: the argument passed to the \":local\" keyword.
REST: all other keywords and their values from the `use-package' invocation.
STATE: idk."
  (let ((body (use-package-process-keywords name rest state)))
    (if (not arg)
        body
      (let* ((all-working-keywords
              (use-package-universal--working-kwds))
             (new-keywords-to-add
              (use-package-universal--ins-nil-after-each-kwd all-working-keywords))
             (filtered-rest
              (use-package-universal--filter-kwds-and-val rest all-working-keywords))
             (new-rest
              (append new-keywords-to-add filtered-rest)))
        (use-package-process-keywords name
          new-rest
          state)))))

(add-to-list 'use-package-keywords :local)

;;; :fetch :::

(defalias 'use-package-normalize/:fetch 'use-package-normalize-predicate)

(defun use-package-handler/:fetch (name keyword arg rest state)
    "Use-package handler for :fetch.
NAME: name of package.
KEYWORD: the \":fetch\" keyword.
ARG: the argument passed to the \":fetch\" keyword.
REST: all other keywords and their values from the `use-package' invocation.
STATE: idk."
  (let ((body (use-package-process-keywords name rest state)))
    (if (not arg)
        body
      (let* ((all-working-keywords
              (use-package-universal--working-kwds))
             (new-keywords-to-add
              (list use-package-universal-fetch-keyword `(,arg)))
             (filtered-rest
              (use-package-universal--filter-kwds-and-val rest all-working-keywords))
             (new-rest
              (append new-keywords-to-add filtered-rest)))
        (use-package-process-keywords name
          new-rest
          state)))))

(add-to-list 'use-package-keywords :fetch)

;;; end
(provide 'use-package--local)
