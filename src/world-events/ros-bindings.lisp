(in-package :event-bullet-world)

(defvar *add-event-msg* (make-fluent :name :new-event) "New event to be checked for")
(defvar *raise-event-msg* (make-fluent :name :new-event) "New event raised")

(defvar *add-event-sub* nil "New event subscriber")
(defvar *add-event-pub* nil "Raised event publisher")

(defun init-ros-event ()
  "Subscribes to topics, binds call backs"
  (setf *add-event-sub* (subscribe "add_event" ros-binding-name #'add-event-cb))
  (setf *event-raised-pub* (advertise "debug" ros-binding-name #'raise-event-pb))
  ; action, service server and client

)

(defun add-event-cb (msg) "Callback for new event values" (setf (value *add-event-msg*) msg))
(defun event-raised-pb (a b c d e f) "Callback for new event values" 
  (publish *add-event-pub* (make-messsage ros-binding-name
                                          :linear) )
