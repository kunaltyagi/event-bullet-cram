(in-package :event-bullet-world)

(defun check-map () ; following ROS conventions on naming nodes
  "Periodically check with a rate to see if some event occured"
  (with-ros-node("event_raising_node" :spin t)
    (init-ros-elements)
    (advertise (get-ros-name "add_event") (get-ros-name "EventUpdate"))
    (ros-warn EVENT-BULLET-WORLD "Event raising node started")
;    (loop-at-most-every 1
      (ros-info EVENT-BULLET-WORLD "Looping")
));)
