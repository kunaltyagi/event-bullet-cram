(in-package :event-bullet-world)

(defparameter ros-binding-base-name "event_bullet_world")
(defun get-ros-name (inp) (concatenate 'string ros-binding-base-name "/" inp))

;(defparameter *add-event-msg* (make-fluent :name :new-event) "New event to be checked for")
;(defparameter *raise-event-msg* (make-fluent :name :new-event) "New event raised")

(defparameter *raise-event-pub* nil) ;; Raised event published by this publisher
(defparameter *add-event-sub* nil) ;; New events to be added read from here

(defun init-ros-elements ()
  "Subscribes to topics, binds call backs"
  (ros-warn EVENT-BULLET-WORLD "1")
;  (setf *raise-event-pub*
;        (advertise (get-ros-name "new_event")
;                            (get-ros-name "EventUpdate"))
;  (setf *add-event-sub*
  (ros-warn EVENT-BULLET-WORLD "2")
        (subscribe (get-ros-name "add_event")
                          (get-ros-name "AddEvent")
                          #'add-event-cb);)
  (register-service "event_status" 'GetEventStatus)
  (ros-warn EVENT-BULLET-WORLD "3")
)

;; @brief Uses the current value (position, velocity or acceleration) of an object
;; with respect to "world" or the target_object depending on is_absolute flag, and
;; returns a function which when called uses the message to determine if the
;; constraint is violated or not
;; @param[in] msg ros-message of type Constraint.msg
;; @return (if (constraint violated) t nil)
;;(defun single-constraint-check (constraint-msg) "Returns a lambda function which checks for this constraint"
;;  (with-fields(target_object source_object constraint_type
;;              is_relative is_magnitude_constraint is_interior is_angluar
;;              data min max) constraint-msg
;;    (setf target_object (if (string= target_object "") "world" target_object))
;;    (let ((msg constraint-msg))
;;      (#'lambda ()
;;                (let ((value (get-current-value msg)))
;;                  ;; @TODO: use is_magnitude, overload < and > for use
;;                  (if is_interior (and (> min value) (< max value)) (or (< min value) (> max value)))))))
;; @TODO: defun position (obj_name), velocity (obj_name), acceleration (obj_name): from prolog? just a thought from projection_demos

;; Slight renaming required. Target object means relative wrt the target object, not wrt the source object

;;(defun get-current-value(msg) "Returns the required relative/absolute value based on target and current objects as well as the constraint_type" (t))

;;  (cond ((= constraint_type POSITION)
;;          (if is_relative (- (position target_object) (position source_object)) (position source_object))
;;        )
;;        ((= constraint_type VELOCITY)
;;          (if is_relative (- (velocity target_object) (velocity source_object)) (velocity source_object))
;;        )
;;        ((= constraint_type ACCELERATION)
;;          (if is_relative (- (acceleration target_object) (acceleration source_object)) (acceleration source_object))
;;        )
;;        (t
;;          (ros-warn EVENT_BULLET_WORLD "Wrong constraint_type provided, falling back to position")
;;          (if is_relative (- (position target_object) (position source_object)) (position source_object)) ;; or call the fn again with constraint_type as POSITION? which is better?
;;)))

(defun add-event-cb (msg) "Callback for new event values" t)
;;  (with-fields (event_name constraints ros_binding_type is_custom) msg
;;    (add-event (make-instance 'world-event
;;                              :event-name event_name
;;                              :response-type ros_binding_type
;;                              :constraint-list constraints
;;                              ;; @TODO: right now returns a list, make it return t or nil
;;                              :raise-event-on-true 
;;      #'(lambda (event) ; no need to use number_of_constraints
;;          ;; USE: https://github.com/mabragor/cl-secure-read ???
;;                (if (is_custom) (eval (read-from-string custom_function)) ;; @TODO: this is clearly wrong
;;                 ((setf (slot-value event 'message (loop for item in (constraint-list event)
;;                                                      collect (single-constraint-check item))))
;;                  (numberp (position t (message event))))))))))

(defun raise-event-pb (msg) "Publishes already prepared messages"
  (publish *raise-event-pub* msg))

;;(defmethod prepare-msg ((event world-event))
;;  (make-message "event_bullet_world/EventUpdate"
;;                :header (make-msg "std_msgs/Header"
;;                                  :stamp (cut:current-timstamp))
;;                :name (event-name event)
;;                :number_of_constraints (length (constraints event))
;;                :status (constraints event)
;;                :has_occured (numberp (position t (message event)))))

;;(def-service-callback GetEventStatus (name)
;;  (make-response "event_bullet_world/EventStatus"
;;                 :header (make-msg "std_msgs/Header"
;;                                   :stamp (cut:current-timstamp))
;;                 ;; @TODO find event using name then use it here as shown
;;                 :name (event-name event) ;; more of confirmation, than requirement
;;                 ;; :has_occured (raise-event-on-true event)
;;                 :number_of_constraints (length (constraints event))
;;                 :status (constraints event)
;;                 :has_occured (numberp (position t (message event)))))
