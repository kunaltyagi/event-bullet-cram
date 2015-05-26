(in-package :event-bullet-world)

(defvar *add-event-msg* (make-fluent :name :new-event) "New event to be checked for")
(defvar *raise-event-msg* (make-fluent :name :new-event) "New event raised")

(defvar *add-event-sub* nil "New event subscriber")
(defvar *raise-event-pub* nil "Raised event publisher")

(defun init-ros-event ()
  "Subscribes to topics, binds call backs"
  (setf *add-event-sub* (subscribe "add_event" ros-binding-name #'add-event-cb))
  (setf *raise-event-pub* (advertise "new_event" ros-binding-name #'raise-event-pb))
  ; action, service server and client

)

;; @TODO: save details of msg, and add the created Event to the list *world-event-accessor-list*
(defun add-event-cb (msg) "Callback for new event values" (setf (value *add-event-msg*) msg))

(defun raise-event-pb (msg) "Publishes already prepared messages"
  (publish *raise-event-pub* msg))

;; @TODO: complete this function
(defmethod prepare-msg ((event world-event))
  t)
