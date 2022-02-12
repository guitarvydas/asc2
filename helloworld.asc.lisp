(proclaim '(optimize (debug 3) (safety 3) (speed 0)))

;;;; named queues
;; in this POC, a named-q is a triple {name message-handler input-q output-q}
;; fifo front is (first q)
;; append is to end of q (last q) (fifo end)
;; (using symbol macros to avoid creating defsetfs)
(defmacro name-field (x) `(first ,x))
(defmacro handler-field (x) `(second ,x))
(defmacro inq-field (x) `(third ,x))
(defmacro outq-field (x) `(fourth ,x))

;; routing descriptor has pair as the sender (component pin)
(defmacro port-field (x) `(first ,x))
(defmacro receiver-name (x) `(first ,x))

(defun find-component-descriptor (target-name queues)
  (assert (not (null queues)))
  (let ((named-q (first queues)))
    (let ((name (name-field named-q)))
      (if (eq name target-name)
	named-q
	(find-component-descriptor target-name (cdr queues))))))

(defun send (port data queues)
  (let ((component (first port)))
    (let ((named-queue (find-component-descriptor component queues)))
      (append-data-to-output-queue named-queue (list port data)))))


(defun dequeue-input-message (named-q)
  (let ((inq (inq-field named-q)))
    (if inq
        (pop (inq-field named-q))
      nil)))

(defun append-data-to-output-queue (named-q event)
  (setf (outq-field named-q)
        (if (null (outq-field named-q))
            (list event)
          (append (outq-field named-q) (list event)))))

(defun enqueue-input-message (message receiver-descriptor)
  ;; input queue is (inq-field receiver-descriptor)
  (setf (inq-field receiver-descriptor)
        (if (null (inq-field receiver-descriptor))
            (list message)
          (append (inq-field receiver-descriptor) (list message)))))

        
(defun find-from (from table)
  (assert (not (null table))) ;; internal error - routing not fully specified
  (let ((routing-descriptor (first table)))
    (let ((to (first (port-field routing-descriptor))))
      (if (eq from to)
          routing-descriptor
        (find-from from (cdr table))))))

(defun copy-message-and-change-pin (message new-pin)
  (let ((data (second message)))
    (list new-pin data)))

(defun route-message (message receivers named-queues)
  (if (null receivers)
      nil
      (let ((receiver (first receivers)))
	(let ((receiver-descriptor (find-component-descriptor (receiver-name receiver) named-queues)))
	  (let ((message-copy (copy-message-and-change-pin message :in)))
	    (enqueue-input-message message-copy receiver-descriptor))
          (route-message message (cdr receivers) named-queues)))))

(defun route-message-to-all-receivers (from message table named-queues)
  ;; a routing descriptor is a 2-tuple { from, to+ }
  ;; where "to" is a list of named-queues (the named-queue for each receiver)
  (let ((routing-descriptor (find-from from table)))
    (let ((receiver-list (second routing-descriptor)))
      (route-message message receiver-list named-queues))))

(defun route-per-sender (from table named-q named-queues)
  (let ((output-queue (outq-field named-q)))
    (if (null output-queue)
        nil
      (let ((output-message (pop (outq-field named-q))))
        (route-message-to-all-receivers from output-message table named-queues)))))

(defun route-messages (table named-queues)
  (if (null named-queues)
      nil
    (let ((named-q (first named-queues)))
      (let ((name (name-field named-q)))
        (route-per-sender name table named-q named-queues)
        (route-messages table (cdr named-queues))))))
  
(defun dispatch-once (named-queues conclude?)
  (let ((queues named-queues))
    (loop
     (unless queues (return)) ;; exit loop
     (when (funcall conclude?) (return))
     (let ((named-q (first queues)))
       (let ((message (dequeue-input-message named-q))
             (handler (handler-field named-q)))
         (when message
           (funcall handler message)))
       (pop queues)))))

(defun dispatch (named-queues routing-table conclude?)
  (loop
   (when (funcall conclude?) (return)) ;; exit loop when done
   (dispatch-once named-queues conclude?)
   (route-messages routing-table named-queues)))
          

(defun default-container-handler (message named-queues)
  (send :self message named-queues))

(defun helloworld ()
  (let (named-queues
	conclude)
    (let ((self-handler (lambda (message) (default-container-handler message named-queues))))
      (let ((conclude-predicate (lambda () conclude)))
        (flet ((not-concluded () (setf conclude nil))
               (concluded () (setf conclude t)))
          (let ((hello (lambda (message)
                         (format *standard-output* "hello gets ~a~%" message)
			 (ecase (first message)
			   (:in
                            (format *standard-output* "hello~%")
                            (send '(hello :out) t named-queues)))))
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
              
              (setf named-queues (list ;; { name inq outq }
                                       (list :self self-handler nil nil)
                                       (list 'hello hello nil nil)
                                       (list 'world world nil nil)))
              (not-concluded)
              (send '(:self :in) t named-queues)
              (route-messages routing-table named-queues)
              (dispatch named-queues routing-table conclude-predicate)
              'done)))))))

