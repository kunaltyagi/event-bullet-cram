(in-package :event-bullet-world)

(defparameter ros-binding-name "event_bullet_world/Event_CRAM")

(defun check-map ((node-name "event_raising_node") (loop-rate 20)) ; following ROS conventions on naming nodes
  "Periodically check with a rate to see if some event occured"
  (with-ros-node(node-name: spin t)
    (ros-warn WORLD-EVENT-CLASS "Event raising node started")
    (loop-at-most-every loop-rate
      ;; Create threads for each object? mskes sense if the number of objects becomes large
      (loop for event in *world-event-accessor-list* when (funcall (raise-event-on-true event))
        do (on-event event)))))

;; For now, lisp is the only way to provide functions
;; @TODO: have functions like: acceleration above threshold, velocity above threshold, position in resricted
;; colume, etc.
;; @TODO: message/service interface for getting above details

