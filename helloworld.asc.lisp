(proclaim '(optimize (debug 3) (safety 3) (speed 0)))

;;;; named queues
;; in this POC, a named-q is a triple {name message-handler input-q output-q}
;; fifo front is (first q)
;; append is to end of q (last q) (fifo end)
;; (using symbol macros to avoid creating defsetfs)
(defmacro part-name-field (x) `(first ,x))
(defmacro part-handler-field (x) `(second ,x))
(defmacro part-inq-field (x) `(third ,x))
(defmacro part-outq-field (x) `(fourth ,x))

;; routing descriptor has pair as the sender (component pin)
(defmacro port-field (x) `(first ,x))
(defmacro receiver-name (x) `(first ,x))
(defmacro port-name (x) `(first ,x))

;; messages
(defmacro message-port (m) `(first ,m))

;; ports
(defmacro port-component (p) `(first ,p))
(defmacro port-tag (p) `(second ,p))

(defun same-port (a b)
  (and (eq (port-component a) (port-component b))
       (eq (port-tag a) (port-tag b))))

(defun find-component-descriptor (target-port queues)
  (assert (not (null queues)))
  (let ((named-q (first queues)))
    (let ((name (part-name-field named-q)))
      (if (eq name (port-name target-port))
	named-q
	(find-component-descriptor target-port (cdr queues))))))

(defun send (port data queues)
  (let ((named-queue (find-component-descriptor port queues)))
    (append-data-to-output-queue named-queue (list port data))))


(defun dequeue-input-message (named-q)
  (let ((inq (part-inq-field named-q)))
    (if inq
        (pop (part-inq-field named-q))
      nil)))

(defun append-data-to-output-queue (named-q event)
  (setf (part-outq-field named-q)
        (if (null (part-outq-field named-q))
            (list event)
          (append (part-outq-field named-q) (list event)))))

(defun enqueue-input-message (message receiver-descriptor)
  ;; input queue is (part-inq-field receiver-descriptor)
  (setf (part-inq-field receiver-descriptor)
        (if (null (part-inq-field receiver-descriptor))
            (list message)
          (append (part-inq-field receiver-descriptor) (list message)))))

        
(defun find-from (sender-port table)
  (assert (not (null table))) ;; internal error - routing not fully specified
  (let ((routing-descriptor (first table)))
    (let ((rd-port (port-field routing-descriptor)))
      (if (same-port sender-port rd-port)
          routing-descriptor
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
  ;; where "to" is a list of parts (the named-queue for each receiver)
  (let ((from-port (message-port message)))
    (let ((routing-descriptor (find-from from-port table)))
      (let ((receiver-list (second routing-descriptor)))
        (route-message message receiver-list parts)))))

(defun route-per-sender (table named-q parts)
  (let ((output-queue (part-outq-field named-q)))
    (if (null output-queue)
        nil
      (let ((output-message (pop (part-outq-field named-q))))
        (route-message-to-all-receivers output-message table parts)))))

(defun route-messages (table parts)
  (if (null parts)
      nil
    (let ((named-q (first parts)))
      (let ((name (part-name-field named-q)))
        (route-per-sender table named-q parts)
        (route-messages table (cdr parts))))))
  
(defun dispatch-once (parts conclude?)
  (let ((queues parts))
    (loop
     (unless queues (return)) ;; exit loop
     (when (funcall conclude?) (return))
     (let ((named-q (first queues)))
       (let ((message (dequeue-input-message named-q))
             (handler (part-handler-field named-q)))
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
            (let ((routing-table
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
              (route-messages routing-table parts)
              (dispatch parts routing-table conclude-predicate)
              'done)))))))

