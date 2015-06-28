(in-package :event-bullet-world)

(defun check-map (&key (loop-rate 10) (node-name "event_raising_node")) ; following ROS conventions on naming nodes
  "Periodically check with a rate to see if some event occured"
;  (defparameter *loop-rate* loop-rate)
  (with-ros-node(node-name :spin t)
    (set-param "loop-rate" (float (/ 1 loop-rate)))
    (init-ros-elements)
    ;(advertise (get-ros-name "add_event") (get-ros-name "EventUpdate"))
    (ros-warn EVENT_BULLET_WORLD "Event raising node started")
    (loop-at-most-every 1
      ;for every event in physics event list,
      ;run it if it is not cancelled and is active
      ;(in a different thread)

      (loop for event in *physics-event-list*
            when (and (not (removal-requested event)) (not (run-status event))) do
        (ros-info EVENT_BULLET_WORLD "Checking ~a event~%" (event-name event))
        (create-thread event)
   ;     if (and (= (removal-requested event) nil) (= (run-status event) t))
   ;       (ros-info EVENT_BULLET_WORLD "Creating thread for ~a event~%" (event-name event)))
;          (make-thread :name (concatenate 'string (event-name event) "-thread")
;            (loop-at-most-every loop-rate
;                                t)))
      )
      (ros-info EVENT_BULLET_WORLD "All events are OK")
      )))
