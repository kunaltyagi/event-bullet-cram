(in-package :event-bullet-world)

(defun check-map (&key (loop-rate 10) (node-name "event_raising_node")) ; following ROS conventions on naming nodes
  "Function which will provide the interface for any other ROS-node"
  (with-ros-node(node-name :spin t)  ; start the node
    (set-param "loop-rate" (float (/ 1 loop-rate)))  ; set the loop-rate for events when they are added
    (init-ros-elements)  ; initialize all ROS interfaces
    (ros-warn EVENT_BULLET_WORLD "Event raising node started")
    (loop-at-most-every 1  ; one second between a re-check of the physics-event list

      (loop for event in *physics-event-list*
            when (and (not (removal-requested event)) (not (run-status event))) do
        (ros-info EVENT_BULLET_WORLD "Checking ~a event~%" (event-name event))
        (create-thread event)  ; create thread IFF required
      )
      (ros-info EVENT_BULLET_WORLD "All events are OK")
)))
