
;; in this POC, a part is a tuple {name message-handler input-q output-q vars}
;; fifo front is (first q)
;; append is to end of q (last q) (fifo end)

;; in the future, all names will be strings, but we allow Lisp symbols, too, in this POC
(defun namify (name)
  (if (symbolp name)
      name
      (if (stringp name)
	  name
	  (assert nil))))

(defun name-eq (name1 name2)
  (if (symbolp name1)
      (eq name1 name2)
      (if (stringp name1)
	  (string= name1 name2)
	  (assert nil))))

;; (the only reason to use macros instead of functions is that macros can be used in setf (this probably argues for getters and setters (defsetf in Lisp)))
(defmacro part-name (x) `(namify (first ,x)))
(defmacro part-handler (x) `(second ,x))
(defmacro part-inq (x) `(third ,x))
(defmacro part-outq (x) `(fourth ,x))

;; connection descriptor has pair as the sender (component pin)
(defmacro connection-sender (x) `(first ,x))
(defmacro connection-receivers (x) `(second ,x))

;; ports
(defmacro port-component (p) `(namify (first ,p)))
(defmacro port-tag (p) `(namify (second ,p)))
(defmacro input-port-component (p) `,(port-component p))
(defmacro input-port-tag (p) `,(port-tag p))
(defmacro output-port-component (p) `,(port-component p))
(defmacro output-port-tag (p) `,(port-tag p))

(defun same-port? (a b)
  (and (name-eq (port-component a) (port-component b))
       (name-eq (port-tag a) (port-tag b))))

;; messages
(defmacro message-kind (m) `(fourth ,m))
(defmacro message-port (m) `(first ,m))
(defmacro message-data (m) `(second ,m))
(defmacro message-tracer (m) `(if (third ,m) (third ,m) nil))
(defmacro message-tag (m) `(port-tag (message-port ,m)))
(defmacro new-message (port data trace kind) `(list ,port ,data ,trace ,kind))

(defun find-part-descriptor (target-port parts)
  (assert (not (null parts)))
  (let ((part (first parts)))
    (let ((name (part-name part)))
      (if (name-eq name (port-component target-port))
	part
	(find-part-descriptor target-port (cdr parts))))))

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

        
(defun find-connection (sender-port table)
  (if (null table)
      (progn
	(format *error-output* "sender ~a not found in connections table~%" sender-port)
	(assert nil)) ;; internal error if sender not found in connections table
    (let ((connection-descriptor (first table)))
      (let ((cd-port (connection-sender connection-descriptor)))
        (if (same-port? sender-port cd-port)
            connection-descriptor
          (find-connection sender-port (cdr table)))))))

(defun copy-message-and-change-port (message new-port)
  (let ((data (message-data message)))
	;(from-port (message-port message)))
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
  ;; route message from sender to all receivers
  ;;
  ;; a routing descriptor is a 2-tuple { from, to+ }
  ;; where "to" is a list of parts (the partueue for each receiver)
  (let ((sender-port (message-port message)))
    (let ((routing-descriptor (find-connection sender-port connections)))
      (let ((receiver-list (connection-receivers routing-descriptor)))
          (route-message message receiver-list parts)))))

(defun route-per-sender (connections part parts)
  ;; if this part has anything on its output queue, route one message to each receiver, repeat
  (let ((output-queue (part-outq part)))
    (if (null output-queue)
        nil
      (let ((output-message (pop (part-outq part))))
        (route-message-to-all-receivers output-message connections parts)))))

(defun route-messages (connections parts all-parts)
  ;; for each part, ...
  (if (null parts)
      nil
    (let ((sender (first parts)))
      (route-per-sender connections sender all-parts)
      (route-messages connections (cdr parts) all-parts))))
  
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
   (route-messages connections parts parts)))
          

(defun default-container-handler (message cause parts)
  (send :self message cause parts))

