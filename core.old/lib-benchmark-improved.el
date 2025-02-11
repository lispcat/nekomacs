
;;; Benchmarking:

(defmacro +benchmark-action (feature action &rest body)
  "A slight modification to the `benchmark-progn' macro.
It now prints FEATURE when printing the time taken to eval BODY.
ACTION can be either \\='require or \\='load, which prepends FEATURE with either
\"Required: \" or \"Loaded: \". If neither match, print \\=`\",ACTION: \"."
  (declare (debug t) (indent defun))
  (let ((value (make-symbol "value"))
	(start (make-symbol "start"))
	(gcs (make-symbol "gcs"))
	(gc (make-symbol "gc")))
    `(let ((,gc gc-elapsed)
	   (,gcs gcs-done)
           (,start (current-time))
           (,value (progn
                     ,@body)))
       (message "Benchmark: Elapsed time: %fs, %s%s%s"
                (float-time (time-since ,start))
		(cond ((eq ,action 'require)
		       "Required: '")
		      ((eq ,action 'load)
		       "Loaded: ")
		      (t (format "%s: " ,action)))
		,feature
                (if (> (- gcs-done ,gcs) 0)
                    (format " (%fs in %d GCs)"
	                    (- gc-elapsed ,gc)
	                    (- gcs-done ,gcs))
                  ""))
       ;; Return the value of the body.
       ,value)))

(provide 'lib-benchmark-improved)
