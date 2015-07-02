(in-package :event-bullet-world)

(defclass object-event (physics-event) ()
  (:documentation "All object generated events, will inherit from other events too"))

(defgeneric add-object-event (event)
  (:documentation "Adds object generated event to internal list"))
(defgeneric get-object-event-list ()
  ;; ROS wrappers required??
  (:documentation "Returns read-write safe copy of internal list"))
(defgeneric eq-object-event (event event)
  ;; replace eq-physics-event and eq-object event with eq-event??
  (:documentation "Checks 2 object generated events based on their names"))

;; this is better than simply maintaining two different lists fo object events and world events since we don't
;; know the complete requirements for obj-events. However, for now, the two are indistinguisible except for the
;; fact that object-events have to call on-event function themselves (somehow) while for world-event objects,
;; the on-event function is called after checking at a specific loop rate

;; @gaya-: (defmethod on-event ((event object-event)) already exists for world-event, so inherited (right??)

;; Objects will just have to call: (on-event obj)

(defparameter *object-event-list* () "Holds all object generated events")
(defparameter *object-read-write-mutex* (make-mutex :name "object-event-list-mutex") "Mutex with one writes, multiple reader lock")

(defmethod add-object-event ((event object-event))
  "Adds object-event to relevant list"
  (ros-info EVENT_BULLET_WORLD "Adding object generated event (~a) to relevant list" (event-name event))
  (with-mutex (*object-read-write-mutex*) (append (list event) *object-event-list*)))

(defmethod get-object-event-list ()
  "Returns deepcopy of list"
  (copy-list *object-event-list*))

(defmethod eq-object-event ((lhs object-event) (rhs object-event))
  "Equality for 2 object generated events, based on name only"
  (string= (event-name lhs) (event-name rhs)))

(defmethod on-event :after ((event object-event))
  "Overloads on-event to return true"
  (append (list (ros-time)) (occurance-stack event))
  (if (not (numberp (position event *object-event-list* :test 'eq-object-event)))
      (add-object-event event))
  t)  ; t to prevent (add-object-event xx) output going to the user

