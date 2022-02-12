(proclaim '(optimize (debug 3) (safety 3) (speed 0)))

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

;;;; named queues
;; in this POC, a named-q is a triple {name input-q output-q}
;; first-in is (first q)
;; append is to end of q (last q)
(defun component-field (named-q)
  (first named-q))

(defun dequeue-input-message (named-q)
  (let ((inq (second named-q)))
    (if inq
        (pop inq)
      nil)))

(defun append-data-to-output-queue (named-q data)
  (setf (third named-q) (if (null (third named-q))
                            (list data)
                          (append (third named-q) (list data)))))

(defun enqueue-input-message (message receiver-descriptor)
  ;; input queue is (second receiver-descriptor)
  (setf (second receiver-descriptor) (if (null (second receiver-descriptor))
                            (list message)
                          (append (second receiver-descriptor) (list message)))))

        
(defun find-from (from table)
  (assert (not (null table))) ;; internal error - routing not fully specified
  (let ((routing-descriptor (first table)))
    (let ((to (first routing-descriptor)))
      (if (eq from to)
          routing-descriptor
        (find-from from (cdr table))))))

(defun route-message (message receivers)
  (if (null receivers)
      nil
    (let ((receiver (first receivers)))
      (enqueue-input-message message receiver))))

(defun route-message-to-all-receivers (from message table)
  ;; a routing descriptor is a 2-tuple { from, to+ }
  ;; where "to" is a list of named-queues (the named-queue for each receiver)
  (let ((routing-descriptor (find-from from table)))
    (let ((receiver-list (second routing-descriptor)))
      (route-message message receiver-list))))

(defun route-per-component (from table named-q)
  (let ((output-queue (third named-q)))
    (if (null output-queue)
        nil
      (let ((output-message (pop (third named-q))))
        (route-message-to-all-receivers from output-message table)))))

(defun route-messages (table named-queues)
  (if (null named-queues)
      nil
    (let ((named-q (first named-queues)))
      (let ((name (first named-q)))
        (route-per-component name table named-q)
        (route-messages table (cdr named-queues))))))
  
(defun dispatch-once (components named-queues conclude?)
  (let ((queues named-queues))
    (assert (= (length components) (length queues)))
    (loop
     (unless components (return)) ;; exit loop
     (when (funcall conclude?) (return))
     (let ((named-q (first queues)))
       (let ((message (dequeue-input-message named-q))
             (component (component-field named-q)))
         (when message
           (funcall component message)))
       (pop queues)
       (pop components)))))

(defun dispatch (components named-queues routing-table conclude?)
  (loop
   (when (funcall conclude?) (return)) ;; exit loop when done
   (dispatch-once components named-queues conclude?)
   (route-messages routing-table named-queues)))
          

(defun helloworld ()
  (let ((named-queues (list ;; { name inq outq }
		       (list :self nil nil)
		       (list 'hello nil nil)
		       (list 'world nil nil)))
	conclude)
    (let ((conclude-predicate (lambda () conclude)))
      (flet ((not-concluded () (setf conclude nil))
             (concluded () (setf conclude t)))
        (let ((hello (lambda (message) (declare (ignore message))
                       (format *standard-output* "hello~%")
                       (send 'hello t named-queues)))
              (world (lambda (message) (declare (ignore message))
                       (format *standard-output* "world~%")
                       (concluded))))
          (let ((routing-table
                 (list ;; { sender (receivers) } 
                       (list :self (list 'hello))
                       (list 'hello (list 'world)))))
            
            (not-concluded)
            (send :self t named-queues)
            (route-messages routing-table named-queues)
            (dispatch (list hello world) named-queues routing-table conclude-predicate)
            'done))))))
      
	    
