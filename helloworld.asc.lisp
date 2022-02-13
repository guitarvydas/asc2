(proclaim '(optimize (debug 3) (safety 3) (speed 0)))

;; in this POC, a part is a tuple {name message-handler input-q output-q vars}
;; fifo front is (first q)
;; append is to end of q (last q) (fifo end)

;; (the only reason to use macros instead of functions is that macros can be used in setf (this probably argues for getters and setters (defsetf in Lisp)))
(defmacro part-name (x) `(first ,x))
(defmacro part-handler (x) `(second ,x))
(defmacro part-inq (x) `(third ,x))
(defmacro part-outq (x) `(fourth ,x))

;; connection descriptor has pair as the sender (component pin)
(defmacro connection-sender (x) `(first ,x))
(defmacro connection-receivers (x) `(first ,x))

;; ports
(defmacro port-component (p) `(first ,p))
(defmacro port-tag (p) `(second ,p))
(defmacro input-port-component (p) `,(port-component p))
(defmacro input-port-tag (p) `,(port-tag p))
(defmacro output-port-component (p) `,(port-component p))
(defmacro output-port-tag (p) `,(port-tag p))

(defun same-port? (a b)
  (and (eq (port-component a) (port-component b))
       (eq (port-tag a) (port-tag b))))

;; messages
(defmacro message-kind (m) `(fourth ,m))
(defmacro message-port (m) `(first ,m))
(defmacro message-data (m) `(second ,m))
(defmacro message-tracer (m) `(if (third ,m) (third ,m) nil))
(defmacro message-tag (m) `(port-tag (message-port ,m)))
(defmacro new-message (port data trace kind) `(list ,port ,data ,trace ,kind))

(defun find-part-descriptor (target-port queues)
  (assert (not (null queues)))
  (let ((part (first queues)))
    (let ((name (part-name part)))
      (if (eq name (port-component target-port))
	part
	(find-part-descriptor target-port (cdr queues))))))

(defun sendk (port data cause queues kind)
  (let ((part (find-part-descriptor port queues)))
    (append-data-to-output-queue part (new-message port data (list cause) kind))))

(defun send (port data cause parts)
  (sendk port data cause parts :async))
(defun send-sync (port data cause parts)
  (sendk port data cause parts :sync))

(defun dequeue-input-message (part)
  (let ((inq (part-inq part)))
    (if inq
        (pop (part-inq part))
      nil)))

(defun append-data-to-output-queue (part event)
  (setf (part-outq part)
        (if (null (part-outq part))
            (list event)
          (append (part-outq part) (list event)))))

(defun enqueue-input-message (message receiver-descriptor)
  ;; input queue is (part-inq receiver-descriptor)
  (setf (part-inq receiver-descriptor)
        (if (null (part-inq receiver-descriptor))
            (list message)
          (append (part-inq receiver-descriptor) (list message)))))

        
(defun find-from (sender-port table)
  (let ((result (find-from1 sender-port table)))
    (assert result) ;; internal error - routing not fully specified
    result))

(defun maybe-find-from (sender-port table)
  (let ((result (find-from1 sender-port table)))
    result))

(defun find-from1 (sender-port table)
  (if (null table)
      nil
    (let ((connection-descriptor (first table)))
      (let ((cd-port (connection-sender connection-descriptor)))
        (if (same-port? sender-port cd-port)
            connection-descriptor
          (find-from1 sender-port (cdr table)))))))

(defun copy-message-and-change-port (message new-port)
  (let ((data (message-data message))
	(from-port (message-port message)))
    (new-message new-port data message :async)))

(defun route-message (message receivers parts)
  (if (null receivers)
      nil
      (let ((receiver (first receivers)))
	(let ((receiver-descriptor (find-part-descriptor receiver parts)))
	  (let ((message-copy (copy-message-and-change-port message receiver)))
	    (enqueue-input-message message-copy receiver-descriptor))
          (route-message message (cdr receivers) parts)))))

(defun route-message-to-all-receivers (message connections parts)
  ;; a routing descriptor is a 2-tuple { from, to+ }
  ;; where "to" is a list of parts (the partueue for each receiver)
  (let ((from-port (message-port message)))
    (let ((routing-descriptor (maybe-find-from from-port connections)))
      (when routing-descriptor
        (let ((receiver-list (second routing-descriptor)))
          (route-message message receiver-list parts))))))

(defun route-per-sender (connections part parts)
  (let ((output-queue (part-outq part)))
    (if (null output-queue)
        nil
      (let ((output-message (pop (part-outq part))))
        (route-message-to-all-receivers output-message connections parts)))))

(defun route-messages (connections parts)
  (if (null parts)
      nil
    (let ((part (first parts)))
      (let ((name (part-name part)))
        (route-per-sender connections part parts)
        (route-messages connections (cdr parts))))))
  
(defun dispatch-once (parts conclude?)
  (let ((queues parts))
    (loop
     (unless queues (return)) ;; exit loop
     (when (funcall conclude?) (return))
     (let ((part (first queues)))
       (let ((message (dequeue-input-message part))
             (handler (part-handler part)))
         (when message
           (funcall handler message)))
       (pop queues)))))

(defun dispatch (parts connections conclude?)
  (loop
   (when (funcall conclude?) (return)) ;; exit loop when done
   (dispatch-once parts conclude?)
   (route-messages connections parts)))
          

(defun default-container-handler (message cause parts)
  (send :self message cause parts))

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
                            (send '(hello :out) t message parts)))))
                (world (lambda (message)
                         (format *standard-output* "world gets ~a~%" message)
			 (ecase (message-tag message)
			   (:in
                            (format *standard-output* "world~%")
                            (concluded))))))
            (let ((connections
                   (list ;; { sender (receivers) } 
                       (list '(:self :in) (list '(hello :in)))
                       (list '(hello :out) (list '(world :in))))))
              
              (setf parts (list ;; { name inq outq }
                                       (list :self self-handler nil nil)
                                       (list 'hello hello nil nil)
                                       (list 'world world nil nil)))
              (not-concluded)
              (send '(:self :in) t nil parts)
              (route-messages connections parts)
              (dispatch parts connections conclude-predicate)
              'done)))))))

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
              (route-messages connections parts)
              (dispatch parts connections conclude-predicate)
              'done)))))))

(defun helloworld6 ()
  (let (parts
	conclude
        result)
    (let ((conclude-predicate (lambda () conclude)))
      (flet ((not-concluded () (setf conclude nil))
             (concluded () (setf conclude t)))
        (let (
              
              (hello (lambda (message)
                       (format *standard-output* "hello gets ~a~%" message)
                       (ecase (message-tag message)
                         (:in
                          (format *standard-output* "hello~%")
                          (send '(hello :out) t message parts)))))
              
              (world (lambda (message)
                       (format *standard-output* "world gets ~a~%" message)
                       (ecase (message-tag message)
                         (:in
                          (format *standard-output* "world~%")
                          (send-sync '(world result) 'eof message parts)
                          (concluded)))))
              
              (self-handler (lambda (message)
                              (default-container-handler message message parts)))
              
              )
          (let ((connections
                 (list ;; { sender (receivers) } 
                       (list '(:self :in) (list '(hello :in)))
                       (list '(hello :out) (list '(world :in))))))
            
            (setf parts (list ;; { name inq outq }
                              (list :self self-handler nil nil)
                              (list 'hello hello nil nil)
                              (list 'world world nil nil)))
            (not-concluded)
            (send '(:self :in) t nil parts)
            (route-messages connections parts)
            (dispatch parts connections conclude-predicate)
            result))))))

(defun get-var (x y) (assert nil)) ;; niy
