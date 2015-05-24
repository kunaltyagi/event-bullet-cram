(in-package :event-bullet-world)

(defclass object-event (world-event))

;; this is better than simply maintaining two different lists fo object events and world events since we don't
;; know the complete requirements for obj-events. However, for now, the two are indistinguisible except for the
;; fact that object-events have to call on-event function themselves (somehow) while for world-event objects,
;; the on-event function is called after checking at a specific loop rate

;(defmethod on-event ((event object-event)) already exists for world-event, so inherited (right??)

;; Objects will just have to call: (on-event obj)

(defparameter *object-event-accessor-list*
  (let ((object-event-list ()) (*write-flag* (mp:make-lock)))
    (list
      #'(lambda (event object-event) (append list(event) object-event-list))
      #'(lambda () (copy-list object-event-list)) ; to prevent editing by anyone else
)))

;; @TODO: add event in the on-event after checking for a new object event
(defmethod add-event (event object-event) (mp:with-lock (*write-flag*) (funcall (first *object-event-accessor-list*))))

(defmethod list-all-events (event object-event) (mp:with-lock (*write-flag*) (funcall (second *object-event-accessor-list*))))

