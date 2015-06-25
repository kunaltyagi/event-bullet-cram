(in-package :event-bullet-world)

(defclass physics-event (event) ; contains creation timestamp in event-timestamp, required: occurance time-stamp
  ((name
    :initarg
    :event-name
    :accessor event-name
    :documentation "Unique Name of the event, for more verbose/specific/debug output")
   (check-to-raise
    :initarg
    :raise-event-on-true
    ;:initform (error "Supply a function to evaluate which on true would raise an event")
    :accessor raise-event-on-true
    :documentation "Function which on evaluating to true results in raising of the required event")
   (source-message
    :initarg
    :source-msg
    :accessor source-msg
    :documentation "The message which created this event")
   (ros-binding
    :initarg
    :response-type
    :initform 0 ;; 1 for message response on topic, 2 for service, etc.
    :accessor response-type
    :documentation "ROS binding to use for raising the event, see the error message on setting response-type to 5")
   (occured-at
    :initarg
    :occurance-stack
    :initform ()
    :reader occurance-stack
    :documentation "List of timestamps at which the event occured. Latest timestamp is appended to the beginning of the list")
   ;; @TODO: do what ros-msg actually should do
   (ros-msg
    :initarg
    :message
    :accessor message
    :documentation "Stores the latest message to be published as per the class details")
   (status
    :initarg
    :run-status
    :accessor run-status
    :documentation "Stores the status of the event. nil signifies passive, t means active")
   (cancel-flag
    :initarg
    :removal-requested
    :accessor removal-requested
    :documentation "t if event is to be moved from active to passive")
))

(defmethod initialize-instance :after ((event physics-event) &key ((:debug debug-mode) 0 debug-mode-supplied-p))
  (case (response-type event)
    (0 (format t "No ROS bindings provided, all clear: ~a event~%" (event-name event)))
    (1 (ros-info EVENT_BULLET_WORLD "Message chosen for communication"))
    (2 (ros-info EVENT_BULLET_WORLD "Polling only support ready for ROS Service"))
    (3 (ros-error EVENT_BULLET_WORLD "No support for ROS Action, fallback to messages")
       (setf (response-type event) 1))
    (4
;; rosparam set to 1 and 0 not true or false due to problems while reading the param
;; into double or int variable in python and c++
       (ros-info EVENT_BULLET_WORLD "Parameter chosen for communication")
       (set-param (event-name event) 0)
       (set-param (concantenate (event-name event) "/status") (message event))) ; set the parameter to 0, so parameter server has the required param, as well as detailed status
    (otherwise (error "Wrong :response-type provided. ROS provides only 4 communication protocols: \n1. Messages\n2. Services\n3. Actions\n4. Parameters\nPlease choose the correct one. No fallback"))))

;; @TODO: compile error
(defmethod eq-physics-event ((lhs physics-event) (rhs physics-event))
  (string= (event-name lhs) (event-name rhs)))

;; @gaya- did you mean this instead of
;; (defmethod on-event :after ((event physics-event))
;(defmethod on-event cat-counter ((event physics-event))
;  (setf (slot-value event 'occured-at (append (list (cut:current-timestamp)) (occurance-stack event))))
;  ;; or should event-timestamp be used for this purpose?
;  (case (response-type event)
;    (0 (format t "~a Event~% occured" (event-name event))
;    (1 (raise-event-pb (prepare-msg (event)))) ; publishing here
;    (2 t); do nothing here, it is a polling only feature
;    (3 (ros-error EVENT_BULLET_WORLD "Actions not supported"))
;    (4 (set-param (event-name event) 1)) ; set parameter true here. Is there a possiblity of informing which constraint was violated?
;    (otherwise error())
;))

(defmethod raise-event-as-fast-as-possible ((event physics-event)) t)

;; (defun velocity ((event physics-event))
;; something using prolog queries just like in cram-projection-demos/src/utilities/objects.lisp

;(defparameter physics-event-list-modified nil)
;(defparameter remove-element-list-modified nil)  ;; to reduce calls on list-all-events
;(defparameter *read-write-mutex* (make-mutex :name "physics-event-list-mutex"))
;(defparameter *delete-mutex* (make-mutex :name "remove-physics-event-list-mutex")))

;; convert to class
;(defparameter *physics-event-accessor-list*
;  (let ((physics-event-list (make-instance 'event-list :list-name "active-list"))
;        (remove-physics-event-list (make-instance 'event-list :list-name "removal-list"))
;    (list
;      #'(lambda ((event physics-event)) (append list(event) physics-event-list)
;        ) ; @gaya-: is there need to make it better? (using cons), also, will deletion of event (by the user) also result in deletion of event from here?? (deep copy issue in lists)
;      ;; remove-if Predicate List
;      #'(lambda ((event physics-event)) (remove-if #'(lambda (x) (eq-physics-event event x)) event physics-event-list))
;      #'(lambda () (copy-list physics-event-list)) ; to prevent editing by anyone else
;
;      #'(lambda ((event physics-event)) (append list(event) remove-physics-event-list))
;      #'(lambda ((event physics-event)) (remove-if #'(lambda (x) (eq-physics-event event x)) remove-physics-event-list))
;      #'(lambda () (copy-list remove-physics-event-list))
;      ; @TODO: update physics-event-thread-status variable 
;)))

;(defmethod add-event ((event physics-event))
;  (setq physics-event-list-modified t)
;  (with-mutex (*read-write-mutex*) (funcall (first *physics-event-accessor-list*) event)))

;; timestamp can also be used to compare, imho, timestamp is better compared to name
;(defmethod remove-events-by-name ((event physics-event))
;  (setq physics-event-list-modified t)
;  (with-mutex (*read-write-mutex*) (funcall (second *physics-event-accessor-list*) event))) ; notice plural form of events used, also, removal is by comparing the name, nothing else

;(defmethod list-all-events ()
;  (with-mutex (*read-write-mutex*) (funcall (third *physics-event-accessor-list*))))

;(defmethod add-event-to-remove ((event physics-event))
;  (setq remove-element-list-modified t)
;  (with-mutex (*delete-mutex*) (funcall (fourth *physics-event-accessor-list*) event)))

;; timestamp can also be used to compare, imho, timestamp is better compared to name
;(defmethod remove-events-to-delete-by-name ((event physics-event))
;  (setq remove-element-list-modified t)
;  (with-mutex (*delete-mutex*) (funcall (fifth *physics-event-accessor-list*) event))) ; notice plural form of events used, also, removal is by comparing the name, nothing else

;(defmethod list-all-events-to-remove ()
;  (with-mutex (*delete-mutex*) (funcall (sixth *physics-event-accessor-list*))))

