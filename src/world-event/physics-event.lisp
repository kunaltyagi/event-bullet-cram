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
    :initform 'raise-event-as-fast-as-possible
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
    :accessor occurance-stack
    :documentation "List of timestamps at which the event occured. Latest timestamp is appended to the beginning of the list")
   ;; @TODO: do what ros-msg actually should do
   (ros-msg
    :initarg
    :message
    :accessor message
    :documentation "Stores the latest message to be published as per the class details")
   (active-status
    :initarg
    :run-status
    :accessor run-status
    :documentation "Stores the status of the event. nil signifies passive, t means active")
   (cancel-flag
    :initarg
    :removal-requested
    :accessor removal-requested
    :documentation "t if event is to be moved from active to passive")
   (constraint-list
    :initarg
    :constraints
    :accessor constraints
    :documentation "list of value of constraints")
   (event-status
    :initarg
    :status
    :accessor status
    :documentation "status of physics event, after the necessary operations")
))

(defmethod initialize-instance :after ((event physics-event) &key ) ;((:debug debug-mode) 0 debug-mode-supplied-p))
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
       (set-param (concatenate (event-name event) "/status") (message event)))  ; set the parameter to 0, so parameter server has the required param, as well as detailed status
    (otherwise (error "Wrong :response-type provided. ROS provides only 4 communication protocols: \n1. Messages\n2. Services\n3. Actions\n4. Parameters\nPlease choose the correct one. No fallback"))))

(defmethod eq-physics-event ((lhs physics-event) (rhs physics-event))
  (string= (event-name lhs) (event-name rhs)))

;; @gaya- did you mean this instead of
(defmethod on-event :after ((event physics-event))
  (append (list (ros-time)) (occurance-stack event))
  ;; or should event-timestamp be used for this purpose?
  (case (response-type event)
    (0 (format t "~a Event~% occured" (event-name event)))
    (1 (raise-event-pb (prepare-msg event)))  ; publishing here
    (2 (ros-info EVENT_BULLET_WORLD "Event occured. Polling data updated")) ; do nothing here, it is a polling only feature
    (3 (ros-error EVENT_BULLET_WORLD "Actions not supported"))
    (4 (set-param (event-name event) 1)
       (set-param (concatenate (event-name event) "/status") (message event)))
    (otherwise (error "Wrong :response-type provided. ROS provides only 4 communication protocols. Choose one of them"))  ; strictly, not required.
))

(defmethod raise-event-as-fast-as-possible ((event physics-event)) t)

(defun get-event-by-name (name)
  (or (get-physics-event-by-name name) (get-other-event-by-name name)))

(defun get-other-event-by-name (name)
  (make-instance 'physics-event
                 :event-name name))

(defparameter *physics-event-list* ())
(defparameter *read-write-mutex* (make-mutex :name "physics-event-list-mutex"))

(defmethod add-physics-event ((event physics-event))
  ;deep copy the event??
  (with-mutex (*read-write-mutex*) (append (list event) *physics-event-list*)))

(defun get-physics-event-by-name (name)
  (loop for event in *physics-event-list* when (string= name (event-name event)) return event))

(defmethod remove-physics-event ((event physics-event))
  (with-mutex (*read-write-mutex*) (remove-if #'(lambda (x) (eq-physics-event event x)) *physics-event-list*)))

(defun remove-event-by-name (name)
  (with-mutex (*read-write-mutex*) (remove-if #'(lambda (x) (string= name (event-name x))) *physics-event-list*)))

(defun get-physics-event-list () (copy-list *physics-event-list*))

;; (defun velocity ((event physics-event))
;; something using prolog queries just like in cram-projection-demos/src/utilities/objects.lisp

