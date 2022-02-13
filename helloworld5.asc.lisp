(defun helloworld5 ()
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
                            (send '(hello :out1) t message parts)
                            (send '(hello :out2) t message parts)))))
                (world1 (lambda (message)
                         (format *standard-output* "world1 gets ~a~%" message)
			 (ecase (message-tag message)
			   (:inw1
                            (format *standard-output* "world1~%")))))
                (world2 (lambda (message)
                         (format *standard-output* "world2 gets ~a~%" message)
			 (ecase (message-tag message)
			   (:inw2
                            (format *standard-output* "world2~%")
                            (concluded))))))
            (let ((connections
                   (list ;; { sender (receivers) } 
                       (list '(:self :in) (list '(hello :in)))
                       (list '(hello :out1) (list '(world1 :inw1)))
                       (list '(hello :out2) (list '(world2 :inw2))))))
              
              (setf parts (list ;; { name inq outq }
                                       (list :self self-handler nil nil)
                                       (list 'hello hello nil nil)
                                       (list 'world1 world1 nil nil)
                                       (list 'world2 world2 nil nil)))
              (not-concluded)
              (send '(:self :in) t nil parts)
              (route-messages connections parts parts)
              (dispatch parts connections conclude-predicate)
              'done)))))))

