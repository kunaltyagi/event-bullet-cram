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
(defun single-constraint-check (constraint-msg)
  "Returns a nil or t based on the constraint being satisfied as per the message"
  (with-fields (reference is_scalar is_interior min max) constraint-msg
    (setf reference (if (string= reference "") "world" reference))
    (if (string= "world" reference)
        ;; @gaya- "Should this not be there??"
        (setf reference (write-to-string *current-bullet-world*)))
    (if (= is_scalar
           (get-constant-value 'event_bullet_world-msg:PhysicsConstraint :TRUE))
           t
           t)
;                (let ((value (get-current-value constraint-msg)))
;                  ;; @TODO: use is_magnitude, overload < and > for use
;                  ;(if is_interior (and (> min value) (< max value)) (or (< min value) (> max value)))
;                  t
;                  )))
)
  t)

(defun get-current-value(msg)
  "Returns the required relative/absolute value based on target and current objects as well as the constraint_type"
  (with-fields (is_relative is_angular constraint_type source_object reference) msg
    (let
      ((function-string
        (concatenate 'string
          (if (= is_angular
                 (get-constant-value 'event_bullet_world-msg:PhysicsConstraint :TRUE))
              "angular-"
              "linear-")
          (cond ((= constraint_type
                    (get-constant-value 'event_bullet_world-msg:PhysicsConstraint :POSITION))
                  "position")
                ((= constraint_type
                    (get-constant-value 'event_bullet_world-msg:PhysicsConstraint :VELOCITY))
                  "velocity")
                ((= constraint_type
                    (get-constant-value 'event_bullet_world-msg:PhysicsConstraint :ACCELERATION))
                  "acceleration")
                (t
                  (ros-warn EVENT_BULLET_WORLD "Wrong constraint_type provided, falling back to position")
                  "position")))))
      (if (= is_relative
             (get-constant-value 'event_bullet_world-msg:PhysicsConstraint :TRUE))
          (- (funcall (read-from-string function-string) source_object)
             (funcall (read-from-string function-string) reference))
          (funcall (read-from-string function-string) source_object)))))

;; @TODO: move to 3d-vector in geometry of roslisp_common
(defun v< (vector-a vector-b)
  "Returns t if (v-norm a) < (v-norm b)"
  (< (v-norm vector-a) (v-norm vector-b)))
(defun v> (vector-a vector-b)
  "Returns t if (v-norm a) > (v-norm b)"
  (> (v-norm vector-a) (v-norm vector-b)))
(defun v= (vector-a vector-b &key (scalar nil))
  "Returns t if (v-norm a) = (v-norm b) if scalar = t"
  (if scalar
    (= (v-norm vector-a) (v-norm vector-b))
    (and (= (x vector-a) (x vector-b))
         (= (y vector-a) (y vector-b))
         (= (z vector-a) (z vector-b)))))

(defun linear-position (object-name)
  "Returns the (x,y,z) position of the object"
  (origin      (pose (object *current-bullet-world* (read-from-string object-name)))))
(defun angular-position (object-name)
  "Returns (x,y,z,w) quaternion of the object"
  (quaternion->axis-angle
    (orientation (pose (object *current-bullet-world* (read-from-string object-name))))))

(defun linear-velocity (object-name)
  "Returns the (x,y,z) position of the object"
  (linear-velocity (object *current-bullet-world* (read-from-string object-name))))
(defun angular-velocity (object-name)
  "Returns (x,y,z,w) quaternion of the object"
  (quaternion->axis-angle
    (linear-velocity (object *current-bullet-world* (read-from-string object-name)))))
(defun linear-acceleration (object-name)
  "Returns the (x,y,z) position of the object"
  (linear-acceleration (object *current-bullet-world* (read-from-string object-name))))
(defun angular-acceleration (object-name)
  "Returns (x,y,z,w) quaternion of the object"
  (quaternion->axis-angle
    (linear-acceleration (object *current-bullet-world* (read-from-string object-name)))))

;; Slight renaming required. Target object means relative wrt the target object, not wrt the source object

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
                                 (prog1 (decoder-helper expression how-many :start i)
                                   (incf i how-many))))
                dimension)))))

(defun add-physics-event-cb (msg)
  "Callback for adding new events and starting a thread for them "
  (ros-info EVENT_BULLET_WORLD "Adding Physics Event to list")
  (with-fields (event_name constraint_list ros_binding_type is_custom
                custom_function is_product_of_sums boolean_expression) msg
    (add-physics-event (make-instance 'physics-event
                          :event-name event_name
                          :response-type ros_binding_type
                          :constraints
                            (coerce constraint_list 'list)
                          :source-msg msg
                          :custom-flag is_custom
                          :is-POS is_product_of_sums
                          :boolean-expression (decode-boolex-data boolean_expression)
                          :custom-function
                            (eval (let ((*read-eval* nil ))
                                    (read-from-string
                                      (concatenate 'string "#'(lambda (event) " custom_function ")" ))))
                          :raise-event-on-true
                            (if (= is_custom
                                   (get-constant-value 'event_bullet_world-msg:AddPhysicsEvent :TRUE))
                              #'(lambda (event)
                                (funcall (custom-function event) event))
; OR
;                                (with-fields (custom_function) (source-msg event)
;                                  (eval (let ((*read-eval*)) (read-from-string custom_function)))))
                              #'(lambda (event)
                                (progn  ; After testing is done, change progn to prog1
                                  (loop for item in (constraints event)
                                    collect (single-constraint-check item))
                                  '(nil nil nil t))))))  ; t is a HACK for TESTING
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
