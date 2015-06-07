(in-package :event-bullet-world)

(defparameter ros-binding-name "event_bullet_world/Event_CRAM")

(defun check-map ((node-name "event_raising_node") (loop-rate 20)) ; following ROS conventions on naming nodes
  "Periodically check with a rate to see if some event occured"
  (with-ros-node(node-name: spin t)
    (ros-warn WORLD-EVENT-CLASS "Event raising node started")
    (loop-at-most-every loop-rate
      ;; @TODO: Create threads for each object? makes sense if the number of objects becomes large
      ;; after adding an event, create a thread with loop-rate
      ;; on asking for deletion, destroy the thread.
      ;; Maintain a seperate list of objects to be destroyed.
      ;; In the thread, look for the event in the to-be-destroyed list, if present,
      ;; exit thread after calling remove fn for that object.
      ;; PS: maintain read-write lock
      (loop for event in *world-event-accessor-list* when (funcall (raise-event-on-true event))
        do (on-event event)))))

; @TODO: make another var in the *world-accessor-list* to denote which all elements
; have a thread associated with them. Those which don't have a thread, get one.
(defvar event-list (list-all-events))
(defvar remove-element-list (list-all-events-to-remove))
(loop for event in (if (= world-event-list-modified t) (setq event-list (list-all-events)) (event-list)
  (if event in (if (= remove-element-list-modified t) (setq remove-element-list (list-all-events-to-remove)) (remove-element-list)))
  (setq event 'removal-requested t)
  (if (= (run-status event) nil)
    (lambda (event)
      (if (= (removal-requested event) nil)
        (if (= (run-status event) nil)
         (make-thread
          (loop-at-most-every loop-rate
            (if (= remove-element-list-modified t) (proc (setq *remove-element-list* (list-all-events-to-remove)) (setq remove-element-list-modified nil)))
            (while (numberp (position event *
              (if (raise-event-on-true event) (on-event event) (t))
        )
      )
    )
  )
)
