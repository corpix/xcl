(defun xcl-run (interpreter tool)
  (let* ((implementation
	  (cond
	   ((eq interpreter :sbcl) "sbcl")
	   ((eq interpreter :ccl) "ccl")
	   (t "sbcl")))
	 (command (format "%s-%s" implementation tool))
	 (output-buffer-name (format "*%s*" command))
	 (errors-buffer-name (format "*%s errors*" command)))
    (async-shell-command
     command
     output-buffer-name
     errors-buffer-name)))

(defun xcl-slynk (&optional interpreter)
  (interactive)
  (xcl-run interpreter "slynk"))

(defun xcl-swank (&optional interpreter)
  (interactive)
  (xcl-run interpreter "swank"))
