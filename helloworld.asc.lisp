(defun find-named-queue (target-name queues)
  (assert (not (null queues)))
  (let ((named-q (first queues)))
    (let ((name (first named-q)))
      (if (eq name target-name)
	named-q
	(find-named-queue target-name (cdr queues))))))

(defun send (component data queues)
  (let ((named-queue (find-named-queue component queues)))
    (append-data-to-output-queue named-queue data)))

(defun helloworld ()
  (let ((named-queues (list
		       (list 'hello nil nil)
		       (list 'world nil nil)))
	conclude)
    (let ((hello (lambda (message)
		   (format *standard-output* "hello~%")
		   (send baton t named-queues)))
	  (world (lambda (message)
		   (format *standard-output* "world~%"))))

      (flet ((dispatch-once (components)
               (let ((queues named-queues))
                 (assert (= (length components) (length queues)))
                 (loop
                   (unless components (return)) ;; exit loop
                   (when conclude (return))
		   (let ((message (dequeue-input-message inq)))
		     (when message
		       (funcall component message)))
                   (pop queues)
                   (pop components))))
             
             (dispatch (components)
               (send match-top-level (list string-to-be-matched memory))
               (setf conclude nil)
               (loop
                 (when conclude (return)) ;; exit loop when done
                 (dispatch-once components)))

      (send 'hello t named-queues)
      (dispatch (list hello world))
      'done)))
      
	    
