(in-package :event-bullet-world)

(defparameter ros-binding-name "event_bullet_world/Event_CRAM")

(defun check-map ((node-name "event_raising_node") (loop-rate 20)) ; following ROS conventions on naming nodes
  "Periodically check with a rate to see if some event occured"
  (with-ros-node(node-name: spin t)
    (ros-warn WORLD-EVENT-CLASS "Event raising node started")
    (loop-at-most-every loop-rate
      ;; @TODO: Create threads for each object? mskes sense if the number of objects becomes large
      ;; How will this work?
      ;; Option 1: for each object create a thread which runs in a
      ;; seperate loop-at-most-every, then, removing an object from the list has to
      ;; be handled
      ;; Option 2: current model: check each object parallely, problem: too many
      ;; thread created and destroyed every cycle.
      (loop for event in *world-event-accessor-list* when (funcall (raise-event-on-true event))
        do (on-event event)))))

