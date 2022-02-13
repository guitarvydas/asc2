(proclaim '(optimize (debug 3) (safety 3) (speed 0)))

;; in this POC, a part is a tuple {name message-handler input-q output-q vars}
;; fifo front is (first q)
;; append is to end of q (last q) (fifo end)
;; (using symbol macros to avoid creating defsetfs)
(defmacro part-name-field (x) `(first ,x))
(defmacro part-handler-field (x) `(second ,x))
(defmacro part-inq (x) `(third ,x))
(defmacro part-outq (x) `(fourth ,x))

;; connection descriptor has pair as the sender (component pin)
(defmacro connection-sender (x) `(first ,x))
(defmacro connection-receivers (x) `(first ,x))

;; messages
(defmacro message-port (m) `(first ,m))
(defmacro message-data (m) `(second ,m))

;; ports
(defmacro port-component (p) `(first ,p))
(defmacro port-tag (p) `(second ,p))

(defun same-port (a b)
  (and (eq (port-component a) (port-component b))
       (eq (port-tag a) (port-tag b))))

(defun find-component-descriptor (target-port queues)
  (assert (not (null queues)))
  (let ((part (first queues)))
    (let ((name (part-name-field part)))
      (if (eq name (port-component target-port))
	part
	(find-component-descriptor target-port (cdr queues))))))

(defun send (port data queues)
  (let ((partueue (find-component-descriptor port queues)))
    (append-data-to-output-queue partueue (list port data))))


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
  (assert (not (null table))) ;; internal error - routing not fully specified
  (let ((connection-descriptor (first table)))
    (let ((cd-port (connection-sender connection-descriptor)))
      (if (same-port sender-port cd-port)
          connection-descriptor
        (find-from sender-port (cdr table))))))

(defun copy-message-and-change-pin (message new-pin)
  (let ((data (second message)))
    (list new-pin data)))

(defun route-message (message receivers parts)
  (if (null receivers)
      nil
      (let ((receiver (first receivers)))
	(let ((receiver-descriptor (find-component-descriptor receiver parts)))
	  (let ((message-copy (copy-message-and-change-pin message (port-tag receiver))))
	    (enqueue-input-message message-copy receiver-descriptor))
          (route-message message (cdr receivers) parts)))))

(defun route-message-to-all-receivers (message table parts)
  ;; a routing descriptor is a 2-tuple { from, to+ }
  ;; where "to" is a list of parts (the partueue for each receiver)
  (let ((from-port (message-port message)))
    (let ((routing-descriptor (find-from from-port table)))
      (let ((receiver-list (second routing-descriptor)))
        (route-message message receiver-list parts)))))

(defun route-per-sender (table part parts)
  (let ((output-queue (part-outq part)))
    (if (null output-queue)
        nil
      (let ((output-message (pop (part-outq part))))
        (route-message-to-all-receivers output-message table parts)))))

(defun route-messages (table parts)
  (if (null parts)
      nil
    (let ((part (first parts)))
      (let ((name (part-name-field part)))
        (route-per-sender table part parts)
        (route-messages table (cdr parts))))))
  
(defun dispatch-once (parts conclude?)
  (let ((queues parts))
    (loop
     (unless queues (return)) ;; exit loop
     (when (funcall conclude?) (return))
     (let ((part (first queues)))
       (let ((message (dequeue-input-message part))
             (handler (part-handler-field part)))
         (when message
           (funcall handler message)))
       (pop queues)))))

(defun dispatch (parts routing-table conclude?)
  (loop
   (when (funcall conclude?) (return)) ;; exit loop when done
   (dispatch-once parts conclude?)
   (route-messages routing-table parts)))
          

(defun default-container-handler (message parts)
  (send :self message parts))

(defun helloworld ()
  (let (parts
	conclude)
    (let ((self-handler (lambda (message) (default-container-handler message parts))))
      (let ((conclude-predicate (lambda () conclude)))
        (flet ((not-concluded () (setf conclude nil))
               (concluded () (setf conclude t)))
          (let ((hello (lambda (message)
                         (format *standard-output* "hello gets ~a~%" message)
			 (ecase (first message)
			   (:in
                            (format *standard-output* "hello~%")
                            (send '(hello :out) t parts)))))
                (world (lambda (message)
                         (format *standard-output* "world gets ~a~%" message)
			 (ecase (first message)
			   (:in
                            (format *standard-output* "world~%")
                            (concluded))))))
            (let ((routing-table
                   (list ;; { sender (receivers) } 
                       (list '(:self :in) (list '(hello :in)))
                       (list '(hello :out) (list '(world :in))))))
              
              (setf parts (list ;; { name inq outq }
                                       (list :self self-handler nil nil)
                                       (list 'hello hello nil nil)
                                       (list 'world world nil nil)))
              (not-concluded)
              (send '(:self :in) t parts)
              (route-messages routing-table parts)
              (dispatch parts routing-table conclude-predicate)
              'done)))))))

(defun helloworld5 ()
  (let (parts
	conclude)
    (let ((self-handler (lambda (message) (default-container-handler message parts))))
      (let ((conclude-predicate (lambda () conclude)))
        (flet ((not-concluded () (setf conclude nil))
               (concluded () (setf conclude t)))
          (let ((hello (lambda (message)
                         (format *standard-output* "hello gets ~a~%" message)
			 (ecase (first message)
			   (:in
                            (format *standard-output* "hello~%")
                            (send '(hello :out1) t parts)
                            (send '(hello :out2) t parts)))))
                (world1 (lambda (message)
                         (format *standard-output* "world1 gets ~a~%" message)
			 (ecase (first message)
			   (:inw1
                            (format *standard-output* "world1~%")))))
                (world2 (lambda (message)
                         (format *standard-output* "world2 gets ~a~%" message)
			 (ecase (first message)
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
              (send '(:self :in) t parts)
              (route-messages connections parts)
              (dispatch parts connections conclude-predicate)
              'done)))))))

