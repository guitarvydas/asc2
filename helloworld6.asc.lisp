(defun helloworld6 ()
  (let (parts
	conclude
        result)
    (let ((conclude-predicate (lambda () conclude)))
      (flet ((not-concluded () (setf conclude nil))
             (concluded () (setf conclude t)))
        (let (
              
              (hello (lambda (message)
                       ;(format *standard-output* "hello gets ~a~%" message)
                       (ecase (message-tag message)
                         (:in
                          (format *standard-output* "hello~%")
                          (send '("hello" :out) t message parts)))))
              
              (world (lambda (message)
                       ;(format *standard-output* "world gets ~a~%" message)
                       (ecase (message-tag message)
                         (:in
                          (format *standard-output* "world~%")
                          (send-sync '("world" "result") "c'est fini" message parts)))))
              
              (self-handler (lambda (message)
                              (format *standard-output* ":self gets ~a~%" message)
			      (cond
				((eq (message-tag message) :in)
                                 (default-container-handler message message parts))
                                ((string= "result" (message-tag message))
                                 (setf result (message-data message))
                                 (concluded)))))
              
              )
          (let ((connections
                 (list ;; { sender (receivers) } 
                       (list '(:self :in) (list '("hello" :in)))
                       (list '("hello" :out) (list '("world" :in)))
                       (list '("world" "result") (list '(:self "result")))
                       )))
            (setf parts (list ;; { name inq outq }
                              (list :self self-handler nil nil)
                              (list "hello" hello nil nil)
                              (list "world" world nil nil)))
            (not-concluded)
            (send '(:self :in) t nil parts)
            (route-messages connections parts parts)
            (dispatch parts connections conclude-predicate)
            result))))))
