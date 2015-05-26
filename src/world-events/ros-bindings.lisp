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
