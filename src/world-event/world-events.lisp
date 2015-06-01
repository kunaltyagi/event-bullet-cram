(in-package :event-bullet-world)

(defclass world-event (event) ; contains creation timestamp in event-timestamp, required: occurance time-stamp
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
    :read occurance-stack
    :documentation "List of timestamps at which the event occured. Latest timestamp is appended to the beginning of the list")
   ;; @TODO: do what ros-msg actually should do
   (ros-msg
    :initarg
    :message
    :accessor message
    :documentation "Stores the latest message to be published as per the class details")
   (constraints
    :initarg
    :constraint-list
    :accessor constraint-list
    :documentation "Stores the list of constraints, in (ready to make ros-msg) list format")
   ))

(defmethod initialize-instance :after ((event world-event) &key ((:debug debug-mode) 0 debug-mode-supplied-p))
  (case (response-type event)
    (0 (format t "No ROS bindings provided, all clear: ~a event~%" (event-name event)))
    (1 (ros-info WORLD-EVENT-CLASS "Message chosen for communication"))
    (2 (ros-info WORLD-EVENT-CLASS "Polling only support ready for ROS Service"))
    (3 progn(
             (ros-error WORLD-EVENT-CLASS "No support for ROS Action, fallback to messages")
             (setf (slot-value event 'response-type 1))))
    (4 progn(
;; rosparam set to 1 and 0 not true or false due to problems while reading the param
;; into double or int variable in python and c++
             (ros-info WORLD-EVENT-CLASS "Parameter chosen for communication")
             (set-param (event-name event) 0)
             (set-param (concantenate (event-name event) "/status") (message event)))) ; set the parameter to 0, so parameter server has the required param, as well as detailed status
    (otherwise error("Wrong :response-type provided. ROS provides only 4 communication protocols: \n1. Messages\n2. Services\n3. Actions\n4. Parameters\nPlease choose the correct one. No fallback"))))

(defmethod eq-world-event ((lhs world-event) (rhs world-event)) (string= (event-name lhs) (event-name rhs)))

;; @gaya- did you mean this instead of
;; (defmethod on-event :after ((event world-event))
(defmethod on-event cat-counter ((event world-event))
  (setf (slot-value event 'occured-at (append (list (cut:current-timestamp)) (occurance-stack event))))
  ;; or should event-timestamp be used for this purpose?
  (case (response-type event)
    (0 (format t "~a Event~% occured" (event-name event))
    (1 (raise-event-pb (prepare-msg (event)))) ; publishing here
    (2 t); do nothing here, it is a polling only feature
    (3 (ros-error WORLD-EVENT-CLASS "Actions not supported"))
    (4 (set-param (event-name event) 1)) ; set parameter true here. Is there a possiblity of informing which constraint was violated?
    (otherwise error())
))

(defmethod raise-event-as-fast-as-possible ((event world-event)) t)
;; (defun velocity ((event world-event))
;; something using prolog queries just like in cram-projection-demos/src/utilities/objects.lisp

;; @TODO: remove race condition using write-flag, (is it done?)
(defparameter *world-event-accessor-list*
  (let ((world-event-list ()) (*write-flag* (mp:make-lock)))
    (list
      #'(lambda ((event world-event))
          (append list(event) world-event-list)
        ) ; @gaya-: is there need to make it better? (using cons), also, will deletion of event (by the user) also result in deletion of event from here??
      #'(lambda ((event world-event)) (remove-if event world-event-list #'eq-world-event))
      #'(lambda () (copy-list world-event-list)) ; to prevent editing by anyone else
)))

(defmethod add-event ((event world-event)) (mp:with-lock (*write-flag*) (funcall (first *world-event-accessor-list*))))

;; timestamp can also be used to compare, imho, timestamp is better compared to name
(defmethod remove-events-by-name ((event world-event)) (mp:with-lock (*write-flag*) (funcall (second *world-event-accessor-list*)))) ; notice plural form of events used, also, removal is by comparing the name, nothing else

(defmethod list-all-events ((event world-event)) (mp:with-lock (*write-flag*) (funcall (third *world-event-accessor-list*))))

