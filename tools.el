(defun slynk (&optional interpreter)
  (interactive)
  (let* ((impl (cond
		((eq interpreter :sbcl) "sbcl")
		((eq interpreter :ccl) "ccl")
		(t "sbcl")))
	 (command (format "%s-slynk" impl))
	 (output-buffer-name (format "*%s*" command))
	 (errors-buffer-name (format "*%s errors*" command)))
    (async-shell-command
     command
     output-buffer-name
     errors-buffer-name)))
