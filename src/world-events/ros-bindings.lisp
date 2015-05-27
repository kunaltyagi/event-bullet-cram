(in-package :event-bullet-world)

(defvar *add-event-msg* (make-fluent :name :new-event) "New event to be checked for")
(defvar *raise-event-msg* (make-fluent :name :new-event) "New event raised")

(defvar *add-event-sub* nil "New event subscriber")
(defvar *raise-event-pub* nil "Raised event publisher")

(defun init-ros-event ()
  "Subscribes to topics, binds call backs"
  (setf *add-event-sub* (subscribe "add_event" ros-binding-name #'add-event-cb))
  (setf *raise-event-pub* (advertise "new_event" ros-binding-name #'raise-event-pb))
  (register-service "event_status" 'GetEventStatus)
)

(defun single-constraint-check (constraint-msg) "Returns a lambda function which checks for this constraint"
  ;; @gaya-: I think, we can remove is_not_range and data field from the Constraint
  ;; message.
  (with-fields(target_object source_object constraint_type
              is_relative is_magnitude_constraint is_interior is_not_range is_angluar
              data min max) constraint-msg
;; set lambda function here
  )
)

;; @TODO: save details of msg, and add the created Event to the list *world-event-accessor-list*
(defun add-event-cb (msg) "Callback for new event values"
  (with-fields (event_name constraints ros_binding_type) msg
    (add-event (make-instance 'world-event
                              :event-name event_name
                              :response-type ros_binding_type
                              :message msg
                              :raise-event-on-true #'(lambda (constraints) ; no need to use number_of_constraints
                                                             (loop for item in constraints
                                                             collect (single-constraint-check item)))))))

;;@TODO: add time-stamp here
(defun raise-event-pb (msg) "Publishes already prepared messages"
  (publish *raise-event-pub* msg))

;; @TODO: add time-stamp here
(defmethod prepare-msg ((event world-event))
  (make-message "event_bullet_world/EventUpdate"
                :header (make-msg "std_msgs/Header"
                                  :stamp 2342)
                :name (event-name event)
                :has_occured (raise-event-on-true event)))

;; @TODO: add time-stamp here
(def-service-callback GetEventStatus (name)
  (make-response "event_bullet_world/EventStatus"
                 :header (make-msg "std_msgs/Header"
                                   :stamp 2342)
                 :has_occured (raise-event-on-true event))) ; @gaya- or use the following
;; (with-fields (status) (message event)
;;   (make-response ...
;;                :status status))
