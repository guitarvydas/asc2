(proclaim '(optimize (debug 3) (safety 3) (speed 0)))
(defun helloworld ()
  (let (parts
	conclude)
    (let ((self-handler (lambda (message) (default-container-handler message message parts))))
      (let ((conclude-predicate (lambda () conclude)))
        (flet ((not-concluded () (setf conclude nil))
               (concluded () (setf conclude t)))
          (let ((hello (lambda (message)
                         (format *standard-output* "hello gets ~a~%" message)
			 (ecase (message-tag message)
			   (:in
                            (format *standard-output* "hello~%")
                            (send '("hello" :out) t message parts)))))
                (world (lambda (message)
                         (format *standard-output* "world gets ~a~%" message)
			 (ecase (message-tag message)
			   (:in
                            (format *standard-output* "world~%")
                            (concluded))))))
            (let ((connections
                   (list ;; { sender (receivers) } 
                       (list '(:self :in) (list '("hello" :in)))
                       (list '("hello" :out) (list '("world" :in))))))
              
              (setf parts (list ;; { name inq outq }
                                       (list :self self-handler nil nil)
                                       (list "hello" hello nil nil)
                                       (list "world" world nil nil)))
              (not-concluded)
              (send '(:self :in) t nil parts)
              (route-messages connections parts parts)
              (dispatch parts connections conclude-predicate)
              'done)))))))

