(in-package :event-bullet-world)

(defun check-map (&key (loop-rate 10) (node-name "event_raising_node")) ; following ROS conventions on naming nodes
  "Periodically check with a rate to see if some event occured"
  (with-ros-node(node-name :spin t)
    (init-ros-elements)
    ;(advertise (get-ros-name "add_event") (get-ros-name "EventUpdate"))
    (ros-warn EVENT_BULLET_WORLD "Event raising node started")
    (loop-at-most-every 1
      (ros-info EVENT_BULLET_WORLD "Looping")
      ;for every event in physics event list,
      ;run it if it is not cancelled and is active
      ;(in a different thread)

;      (loop for event in *physics-event-list*
;        (ros-info EVENT_BULLET_WORLD "Checking ~a event~%" (event-name event))
   ;     if (and (= (removal-requested event) nil) (= (run-status event) t))
   ;       (ros-info EVENT_BULLET_WORLD "Creating thread for ~a event~%" (event-name event)))
;      (ros-info EVENT_BULLET_WORLD "All events are OK")
;          (make-thread :name (concatenate 'string (event-name event) "-thread")
;            (loop-at-most-every loop-rate
;                                t)))
      )))
