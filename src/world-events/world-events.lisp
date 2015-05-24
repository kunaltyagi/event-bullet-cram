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
   (topic
    :initarg
    :event-platform
    :initform "debug" ;; node-name prefixed autoamtically by ROS, if not, change it to/event_bullet_world/debug
    :accessor event-platform
    :documentation "In case of messages, the topic to publish on, etc.")
   (ros-binding
    :initarg
    :response-type
    :initform 0 ;; 1 for message response on topic, 2 for service, etc.
    :accessor repsonse-type
    :documentation "ROS binding to use for raising the event, see the error message on setting response-type to 5"))
   (occured-at
    :initarg
    :occurance-stack
    :initform ()
    :read occurance-stack
    :documentation "List of timestamps at which the event occured. Latest timestamp is appended to the beginning of the list"))

(defmethod initialize-instance :after ((event world-event) &key ((:debug debug-mode) 0 debug-mode-supplied-p))
  (case (response-type event)
    (0 (format t "No ROS bindings provided, all clear: ~a event~%" (event-name event))
    ;; I don't know if it is possible to use certain ros commands outside the `with-ros-node`
    ;; So, left the implementation here
    ;; Also, see the proposed message/service/action for raising event, in case of parameter, just set the
    ;; value to 1 (not true, true plays havoc in case it is by mistake read into a double variable, no problems
    ;; with 1 in reading its value to a boolean, integer or double)
    (1 ros-info WORLD-EVENT-CLASS "Message chosen for communication") ; prepare the required message
    (2 ros-info WORLD-EVENT-CLASS "Service chosen for communication") ; prepare the required service
    (3 ros-info WORLD-EVENT-CLASS "Action chosen for communciation")  ; prepare the required action
    (4 ros-info WORLD-EVENT-CLASS "Parameter chosen for communication") ; set the parameter to 0, so parameter server has the required param
    (otherwise error("Wrong :response-type provided. ROS provides only 4 communication protocols: \n1. Messages\n2. Services\n3. Actions\n4. Parameters\nPlease choose the correct one"))))

(defmethod eq-world-event ((lhs world-event) (rhs world-event)) (string= (event-name lhs) (event-name rhs)))

(defmethod on-event cat-counter ((event world-event)) ;; @Gaya: this is correct usage, right??
  (setf (slot-value event 'occured-at (append (list (cut:current-timestamp)) (occurance-stack event))))
  ;; or should event-timestamp be used for this purpose?
  (case (response-type event)
    ;; @TODO: big one
    (0 t); do noting, print??
    (1 t); publish here
    (2 t); call server here
    (3 t); call server here
    (4 t); set parameter true here
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
        ) ; @Gaya: is there need to make it better? (using cons), also, will deletion of event (by the user) also result in deletion of event from here??
      #'(lambda ((event world-event)) (remove-if event world-event-list #'eq-world-event))
      #'(lambda () (copy-list world-event-list)) ; to prevent editing by anyone else
)))

(defmethod add-event ((event world-event)) (mp:with-lock (*write-flag*) (funcall (first *world-event-accessor-list*))))

;; timestamp can also be used to compare, imho, timestamp is better compared to name
(defmethod remove-events-by-name ((event world-event)) (mp:with-lock (*write-flag*) (funcall (second *world-event-accessor-list*)))) ; notice plural form of events used, also, removal is by comparing the name, nothing else

(defmethod list-all-events ((event world-event)) (mp:with-lock (*write-flag*) (funcall (third *world-event-accessor-list*))))

