(in-package :event-bullet-world)

; (defparameter *add-event-msg* (make-fluent :name :new-event) "New event to be checked for")
; (defparameter *raise-event-msg* (make-fluent :name :new-event) "New event raised")
(defun get-constant-value (msg slot)  ;; @TODO: convert to macro
  (roslisp-msg-protocol:symbol-code msg slot))

(defparameter *raise-event-pub* nil "Raised event published by this publisher")
; (defparameter *add-event-sub* nil "New events to be added read from here")

(defun init-ros-elements ()
  "Subscribes to topics, binds service server call backs"
  (setf *raise-event-pub* (advertise (get-node-name "event_update") (get-ros-name "EventUpdate")))
  (subscribe (get-node-name "physics/add_event") (get-ros-name "AddPhysicsEvent") #'add-physics-event-cb)
  (register-service (get-node-name "event_status") 'event_bullet_world-srv:EventStatus)
)

;; @brief Uses the current value (position, velocity or acceleration) of an object
;; with respect to "world" or the target_object depending on is_absolute flag, and
;; returns a function which when called uses the message to determine if the
;; constraint is violated or not
;; @param[in] msg ros-message of type Constraint.msg
;; @return (if (constraint violated) t nil)
(defun single-constraint-check (constraint-msg) "Returns a lambda function which checks for this constraint"
  (with-fields(target_object source_object constraint_type
              is_relative is_scalar is_interior is_angular
              min max) constraint-msg
    (setf target_object (if (string= target_object "") "world" target_object))
;    (ros-info EVENT_BULLET_WORLD "Checking constraint")
;    (let ((msg constraint-msg))
;      (#'lambda ()
;                (let ((value (get-current-value msg)))
;                  ;; @TODO: use is_magnitude, overload < and > for use
;                  ;(if is_interior (and (> min value) (< max value)) (or (< min value) (> max value)))
;                  t
;                  )))))
)
  t)

; @TODO: defun position (obj_name), velocity (obj_name), acceleration (obj_name): from prolog? just a thought from projection_demos

;; Slight renaming required. Target object means relative wrt the target object, not wrt the source object

(defun get-current-value(msg) "Returns the required relative/absolute value based on target and current objects as well as the constraint_type"
  t)
;  (cond ((= constraint_type POSITION)
;          (if is_relative (- (position source_object) (position target_object)) (position source_object))
;        )
;        ((= constraint_type VELOCITY)
;          (if is_relative (- (velocity source_object) (velocity target_object)) (velocity source_object))
;        )
;        ((= constraint_type ACCELERATION)
;          (if is_relative (- (acceleration source_object) (acceleration target_object)) (acceleration source_object))
;        )
;        (t
;          (ros-warn EVENT_BULLET_WORLD "Wrong constraint_type provided, falling back to position")
;          (if is_relative (- (position source_object) (position target_object)) (position source_object)) ;; or call the fn again with constraint_type as POSITION? which is better?
;)))

(defun decoder-helper (input how-many &key start)
  "Takes a list and returns how-many elements starting from :start"
  ;; @TODO: replace (nth i input) with (nth (nth i input) (constraint-list event))
  (loop for i from start to (+ start how-many -1) collect (nth i input)))

(defun decode-boolex-data (bool-ex)
  "Takes an multiarray msg (from ROS) as input, as returns a list suitable for lisp"
  (with-fields (data layout) bool-ex
    (with-fields (dim) layout
      (let* ((expression (coerce data 'list))
             (dimension (loop for gp in (coerce dim 'list)
                          collect (with-fields (size) gp size)))
            )
        ;; for every entry in dimension, collect that many from layout
        (mapcar (let ((i 0)) #'(lambda (how-many)
                                 (prog1 (decode-helper expression how-many :start i)
                                   (incf i how-many))))
                dimension)
      ))

  ))

(defun add-physics-event-cb (msg)
  "Callback for adding new events and starting a thread for them "
  (ros-info EVENT_BULLET_WORLD "Adding Physics Event to list")
  (with-fields (event_name constraint_list ros_binding_type is_custom constraint_relation
                custom_function) msg
;                custom_function is_product_of_sums boolean_expression) msg
    (add-physics-event (make-instance 'physics-event
                          :event-name event_name
                          :response-type ros_binding_type
                          :constraints
                            (coerce constraint_list 'list)
                          :constraint-relation
                            (coerce constraint_relation 'list)
                          :source-msg msg
                          :custom-flag is_custom
;                          :is-POS is_product_of_sums
;                          :boolean-expression (decode-boolex-data boolean_expression)
                          :custom-function
                            (eval (let ((*read-eval* nil ))
                                    (read-from-string
                                      (concatenate 'string "#'(lambda (event) " custom_function ")" ))))
;; @TODO: right now returns a list, make it return t or nil
                          :raise-event-on-true
                            (if (= is_custom
                                   (get-constant-value 'event_bullet_world-msg:AddPhysicsEvent :TRUE))
                              #'(lambda (event)
                                (funcall (custom-function event) event))
; OR
;                                (with-fields (custom_function) (source-msg event)
;                                  (eval (let ((*read-eval*)) (read-from-string custom_function)))))
                              #'(lambda (event)
                                (loop for item in (constraints event)
                                  collect (single-constraint-check item))
                                t))))  ; t is a HACK for TESTING
    (let ((event (get-event-by-name event_name)))
;      (ros-info EVENT_BULLET_WORLD "~a Event added, ~a bindings. Running checks now" (event-name event) (response-type event))
      (create-thread event)))
)

(defun raise-event-pb (msg)
  "Publishes already prepared messages"
  (publish *raise-event-pub* msg))

(def-service-callback event_bullet_world-srv:EventStatus (name)
  "Callback which receives service calls and responds in kind"
  (let ((event (get-event-by-name name)))
    (make-response
                   :header (make-msg "std_msgs/Header"
                                     :stamp (ros-time)
                                     :seq (length (occurance-stack event))
;                                     :frame_id (get-node-name (get-ros-name *current-bullet-world* ))
                                     )
                   :name (event-name event)
                   :number_of_constraints (length (constraints event))
                   :status (make-array (length (constraints event)) :initial-contents (loop for x in (constraints event) collect (bool-to-num x)))
                   :has_occured (bool-to-num (status event)))))
