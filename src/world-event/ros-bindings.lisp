(in-package :event-bullet-world)

; (defparameter *add-event-msg* (make-fluent :name :new-event) "New event to be checked for")
; (defparameter *raise-event-msg* (make-fluent :name :new-event) "New event raised")
(defun get-constant-value (msg slot)
  (roslisp-msg-protocol:symbol-code msg slot))

(defparameter *raise-event-pub* nil "Raised event published by this publisher")
; (defparameter *add-event-sub* nil "New events to be added read from here")

(defun init-ros-elements ()
  "Subscribes to topics, binds service server call backs"
  (setf *raise-event-pub* (advertise (get-ros-name "event_update") (get-ros-name "EventUpdate")))
  (subscribe (get-ros-name "physics/add_event") (get-ros-name "AddPhysicsEvent") #'add-physics-event-cb)
  (register-service "~/event_status" 'event_bullet_world-srv:EventStatus)
)

;; @brief Uses the current value (position, velocity or acceleration) of an object
;; with respect to "world" or the target_object depending on is_absolute flag, and
;; returns a function which when called uses the message to determine if the
;; constraint is violated or not
;; @param[in] msg ros-message of type Constraint.msg
;; @return (if (constraint violated) t nil)
(defun single-constraint-check (constraint-msg) "Returns a lambda function which checks for this constraint"
  (with-fields(target_object source_object constraint_type
              is_relative is_scalar is_interior is_angular
              min max) constraint-msg
    (setf target_object (if (string= target_object "") "world" target_object))
;    (ros-info EVENT_BULLET_WORLD "Checking constraint")
;    (let ((msg constraint-msg))
;      (#'lambda ()
;                (let ((value (get-current-value msg)))
;                  ;; @TODO: use is_magnitude, overload < and > for use
;                  ;(if is_interior (and (> min value) (< max value)) (or (< min value) (> max value)))
;                  t
;                  )))))
)
  t)

; @TODO: defun position (obj_name), velocity (obj_name), acceleration (obj_name): from prolog? just a thought from projection_demos

;; Slight renaming required. Target object means relative wrt the target object, not wrt the source object

(defun get-current-value(msg) "Returns the required relative/absolute value based on target and current objects as well as the constraint_type"
  t)
;  (cond ((= constraint_type POSITION)
;          (if is_relative (- (position source_object) (position target_object)) (position source_object))
;        )
;        ((= constraint_type VELOCITY)
;          (if is_relative (- (velocity source_object) (velocity target_object)) (velocity source_object))
;        )
;        ((= constraint_type ACCELERATION)
;          (if is_relative (- (acceleration source_object) (acceleration target_object)) (acceleration source_object))
;        )
;        (t
;          (ros-warn EVENT_BULLET_WORLD "Wrong constraint_type provided, falling back to position")
;          (if is_relative (- (position source_object) (position target_object)) (position source_object)) ;; or call the fn again with constraint_type as POSITION? which is better?
;)))

(defun add-physics-event-cb (msg)
  "Callback for adding new events and starting a thread for them "
  (ros-info EVENT_BULLET_WORLD "Adding Physics Event to list")
  (with-fields (event_name constraint_list ros_binding_type is_custom constraint_relation
                custom_function) msg
    (add-physics-event (make-instance 'physics-event
                              :event-name event_name
                              :response-type ros_binding_type
                              :constraints (coerce constraint_list 'list)
                              :constraint-relation (coerce constraint_relation 'list)
                              :source-msg msg
                              ;; @TODO: right now returns a list, make it return t or nil
                              :raise-event-on-true
      (if (= is_custom (get-constant-value 'event_bullet_world-msg:AddPhysicsEvent :TRUE))
        #'(lambda (event)
          (with-fields (custom_function) (source-msg event)
            ;; USE: https://github.com/mabragor/cl-secure-read ???
            (eval (read-from-string custom_function))))  ; can also save the function to save overhead using:
;  need to create a custom-function slot in physics-event
;  (setf (custom-function event) (concatenate 'string "#'(lambda () " custom_function ")" ))
;  then using
;  (funcall (custom-function event))
;  which is better??
        #'(lambda (event)
;          (ros-info EVENT_BULLET_WORLD "Single check")
          (loop for item in (constraints event)
            collect (single-constraint-check item))
                  t))))  ; t is a HACK for TESTING
  (let ((event (get-event-by-name event_name)))
  ;  (ros-info EVENT_BULLET_WORLD "~a Event added, ~a bindings. Running checks now" (event-name event) (response-type event))
    (create-thread event)))
)

(defun raise-event-pb (msg)
  "Publishes already prepared messages"
  (ros-info EVENT_BULLET_WORLD "Single check")
  (publish *raise-event-pub* msg))

(def-service-callback event_bullet_world-srv:EventStatus (name)
  "Callback which receives service calls and responds in kind"
  (let ((event (get-event-by-name name)))
    (make-response
                   :header (make-msg "std_msgs/Header"
                                     :stamp (ros-time))
                   :name (event-name event)
                   :number_of_constraints (length (constraints event))
                   :status (constraints event)
                   :has_occured (status event))))
