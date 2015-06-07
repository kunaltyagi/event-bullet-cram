(in-package :event-bullet-world)

(defclass event-list
  ((name
    :initarg
    :list-name
    :accessor list-name
    :documentation "differentiates the mutex")
   (element-added
    :initarg 
    :element-added
    :accessor element-added
    :documentation "t if element added to list and not all readers have been notified, nil otherwise")
   (read-write-mutex
    :initarg
    :read-write-mutex
    :accessor read-write-mutex
    :documentation "either n number of readers exist or one writer exist, across all threads at the same time")
   (element-removed
    :initarg
    :element-removed
    :accessor element-removed
    :documentation "t if element removed form list and not all readers have been notified, nil otherwise")
   (element-list
    :initarg
    :element-list
    :initform ()
    :reader element-list ;; @TODO
    :writer (with-mutex (read-write-mutex) (setf element-list))
    :documentation "read and write accessors to ensure thread-safety")
))

(defmethod initialize-instance :after ((new-list event-list) &key)
  (setf (read-write-mutex new-list) (make-mutex :name (concatenate 'string name "-mutex")))
)

(defmethod get-element-copy ((new-list event-list))
  (copy-list (element-list new-list)))

