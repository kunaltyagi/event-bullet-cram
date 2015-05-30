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

;; @TODO: save details of msg, and add the created Event to the list *world-event-accessor-list*
(defun add-event-cb (msg) "Callback for new event values" (setf (value *add-event-msg*) msg))
;; @TODO: defun position (obj_name), velocity (obj_name), acceleration (obj_name): from prolog? just a thought from projection_demos
;; if empty, take obj_name to be "world" instead

;; Slight renaming required. Target object means relative wrt the target object, not wrt the source object

;; @TODO: remove is_range, not using it
;; @TODO: use is_magnitude, overload < and > for use
;; (let (value (get-current-value msg))
;;   (if is_interior (and (> min value) (< max value)) (or (< min value) (> max value))))

;; (defun get-current-value(msg) "Returns the required relatice/absolute value based on target and current objects as well as the constraint_type"
;;   (cond ((= constraint_type POSITION)
;;           (if is_relative (- (position target_object) (position source_object)) (position source_object))
;;         )
;;         ((= constraint_type VELOCITY)
;;           (if is_relative (- (velocity target_object) (velocity source_object)) (velocity source_object))
;;         )
;;         ((= constraint_type ACCELERATION)
;;           (if is_relative (- (acceleration target_object) (acceleration source_object)) (acceleration source_object))
;;         )
;;         (t 
;;           (ros-warn EVENT_BULLET_WORLD "Wrong constraint_type provided, falling back to position")
;;           (if is_relative (- (position target_object) (position source_object)) (position source_object)) ;; or call the fn again with constraint_type as POSITION? which is better?
;;         ))

(defun raise-event-pb (msg) "Publishes already prepared messages"
  (publish *raise-event-pub* msg))

;; @TODO: add time-stamp here
(defmethod prepare-msg ((event world-event))
  (make-message "event_bullet_world/EventUpdate"
                :header (make-msg "std_msgs/Header"
                                  :stamp 2342)
                :name (event-name event)
                :status (raise-event-on-true event)))

;; @TODO: add time-stamp here
(def-service-callback GetEventStatus (name)
  (make-response "event_bullet_world/EventStatus"
                 :header (make-msg "std_msgs/Header"
                                   :stamp 2342)
                 :status (raise-event-on-true event)))
