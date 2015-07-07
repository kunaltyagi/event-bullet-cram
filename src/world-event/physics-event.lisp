(in-package :event-bullet-world)

; start generic stuff
; PS: to view details of objects, use (describe <obj_name>)
; can be put into a more basic lisp file
(defparameter ros-binding-base-name "event_bullet_world")
(defun get-ros-name (inp)
  "Gets the correct name for ROS related inp related to this package"
  (concatenate 'string ros-binding-base-name "/" inp))
(defun get-node-name (inp)
  "Gets the absolute name of inp (inp is relative to the node"
  (concatenate 'string *ros-node-name* "/" inp))
(defun bool-to-num (inp)
  "Returns 1 for t and 0 for nil"
  (if inp 1 0))
; end generic stuff

; start class physics-event related stuff
(defclass physics-event (event)
  ((name
    :initarg
    :event-name
    :accessor event-name
    :initform "dummy"
    :documentation "Unique Name of the event, for more verbose/specific/debug output")
   (check-to-raise
    :initarg
    :raise-event-on-true
    ;:initform (error "Supply a function to evaluate which on true would raise an event")
    :initform 'raise-event-as-fast-as-possible
    :accessor raise-event-on-true
    :documentation "Function which on evaluating to true results in raising of the required event")
   (source-message
    :initarg
    :source-msg
    :accessor source-msg
    :documentation "The message which created this event")
   (ros-binding
    :initarg
    :response-type
    :initform 0
    :accessor response-type
    :documentation "ROS binding to use for raising the event, see the error message on setting response-type to 5")
   (occured-at
    :initarg
    :occurance-stack
    :initform ()
    :accessor occurance-stack
    :documentation "List of timestamps at which the event occured. Latest timestamp is appended to the beginning of the list")
   ;; @TODO: do what ros-msg actually should do
   (ros-msg
    ; no need currently. Maybe required in a race condition. Not sure
    ; @TODO: update to (message event) should happen via check-to-raise IMHO
    :initarg
    :message
    :accessor message
    :documentation "Stores the latest message to be published as per the class details")
   (active-status
    :initarg
    :run-status
    :initform t
    :accessor run-status
    :documentation "Stores the status of the event. nil signifies passive, t means active")
   (cancel-flag
    :initarg
    :removal-requested
    :initform nil
    :accessor removal-requested
    :documentation "t if event is to be moved from active to passive")
   (constraint-list
    :initarg
    :constraints
    :initform ()
    :accessor constraints
    :documentation "list of value of constraints")
   (constraint-relations
    :initarg
    :constraint-relation
    :initform ()
    :accessor constraint_relation
    :documentation "DEPRECIATED: boolean relations between the constraints")
   (boolean-expression
    :initarg
    :boolean-expression
    :initform ()
    :accessor boolean-expression
    :documentation "List of lists with encoded boolean expression of relation between constraints")
   (is-POS
    :initarg
    :is-POS
    :initform nil
    :accessor is-POS
    :documentation "SOP or POS form used for encoding boolean-expression")
   (event-status
    :initarg
    :status
    :initform nil
    :accessor status
    :documentation "status of physics event, after the necessary operations")
   (custom-flag
    :initarg
    :custom-flag
    :initform nil
    :accessor custom-flag
    :documentation "Flag is true if custom command is provided to be evaluated")
   (custom-function
    :initarg 
    :custom-function
    :initform 'raise-event-as-fast-as-possible
    :accessor custom-function
    :documentation "Stores the custom function to be called if custom-flag is true")
  )
  (:documentation "contains creation timestamp in event-timestamp, required: occurance time-stamp"))

(defmethod initialize-instance :after ((event physics-event) &key ) ;((:debug debug-mode) 0 debug-mode-supplied-p))
  "sets some required parameters on ROS server and ros-info for run-time ACK"
  (case (response-type event)
    (0 (format t "No ROS bindings provided, all clear: ~a event~%" (event-name event)))
    (1 (ros-info EVENT_BULLET_WORLD "Message chosen for communication"))
    (2 (ros-info EVENT_BULLET_WORLD "Polling only support ready for ROS Service"))
    (3 (ros-error EVENT_BULLET_WORLD "No support for ROS Action, fallback to messages")
       (setf (response-type event) 1))
    (4
;; rosparam set to 1 and 0 not true or false due to problems while reading the param
;; into double or int variable in python and c++
       (ros-info EVENT_BULLET_WORLD "Parameter chosen for communication")
       (set-param (event-name event) 0)
       (set-param (concatenate (event-name event) "/status") (message event)))  ; set the parameter to 0, so parameter server has the required param, as well as detailed status
    (otherwise (error "Wrong :response-type provided. ROS provides only 4 communication protocols: \n1. Messages\n2. Services\n3. Actions\n4. Parameters\nPlease choose the correct one. No fallback"))))

(defmethod on-event :after ((event physics-event))
  "After on-event is called, do the necessary stuff via ROS"
  (append (list (ros-time)) (occurance-stack event))
  (case (response-type event)
    (0 (format t "~a Event occured~%" (event-name event)))
    (1 ;(ros-info EVENT_BULLET_WORLD "Publishing message") (prepare-msg event))
       (raise-event-pb (prepare-msg event)))  ; should a saved message be published? Makes more sense, but a change of only 4 instructions
       ;(raise-event-pb (message event)))
    (2 (ros-info EVENT_BULLET_WORLD "Event occured. Polling data updated")) ; do nothing here, it is a polling only feature
    (3 (ros-error EVENT_BULLET_WORLD "Actions not supported"))
    (4 (set-param (event-name event) 1)
       (set-param (concatenate (event-name event) "/status") (message event)))
    (otherwise (error "Wrong :response-type provided. ROS provides only 4 communication protocols. Choose one of them"))  ; strictly, not required.
))

(defgeneric raise-event-as-fast-as-possible (event)
  (:documentation "Sends true for any call with any event as input"))
(defgeneric never-raise-event (event)
  (:documentation "Never send true for any event"))
(defgeneric eq-physics-event (event event)
  (:documentation "Equate lhs and rhs based on their name"))
(defgeneric add-physics-event (event)
  (:documentation "Add event to internal list which runs at 1 Hz"))
(defgeneric remove-physics-event (event)
  ;; @TODO: implement ROS wrappers for this
  (:documentation "Set event as passive. It will not be checked until activated"))
(defgeneric single-check (event)
  (:documentation "Runs in a loop, for checking the status of the event once. Calls required function internally"))
(defgeneric create-thread (event)
  (:documentation "Creates thread for the event and runs thread-function (with mutex?)"))
(defgeneric thread-function (event)
  (:documentation "Called by create-thread function. Handles implementation, runs single-check till the event is active"))
(defgeneric prepare-msg (event)
  (:documentation "Returns EventUpdate ROS msg for event"))

(defmethod eq-physics-event ((lhs physics-event) (rhs physics-event))
  "Equality for 2 physics-event is based on their names, nothing else"
  (string= (event-name lhs) (event-name rhs)))

(defmethod raise-event-as-fast-as-possible ((event physics-event)) t)
; (defun raise-event-as-fast-as-possible () t)
(defmethod never-raise-event ((event physics-event)) nil)

(defun get-event-by-name (name)
  "Returns the event provided the name is there on some list"
  (or (get-physics-event-by-name name) (get-other-event-by-name name)))

(defun get-other-event-by-name (name)
  "Will incorporate calls to object-event and logical-event functions"
  (make-instance 'physics-event
                 :event-name name))

(defparameter *physics-event-list* () "All physics events created are here")  ; move onto Hash tables for large no. of events
(defparameter *physics-read-write-mutex* (make-mutex :name "physics-event-list-mutex") "Mutex with one writer OR infi reader concept")

(defmethod add-physics-event ((event physics-event))
  "Add the physics-event to the list"
  ;@Gaya- : deep copy the event??
  (with-mutex (*physics-read-write-mutex*)
    (setf *physics-event-list* (nconc *physics-event-list* (list event)))))

(defun get-physics-event-by-name (name)
  "Return nil or a physics-event with the provided name"
  (loop for event in *physics-event-list* when (string= name (event-name event)) return event))

(defmethod remove-physics-event ((event physics-event))
  "Removes the event from the list (comparison by name only)"
  (with-mutex (*physics-read-write-mutex*) (remove-if #'(lambda (x) (eq-physics-event event x)) *physics-event-list*)))

(defun remove-event-by-name (name)
  "Removes the event with the same name from the list"
  (with-mutex (*physics-read-write-mutex*) (remove-if #'(lambda (x) (string= name (event-name x))) *physics-event-list*)))

(defun get-physics-event-list ()
  "Returns a deep-copy of the list for use by anyone else, to be used for query over ROS for existing events"
  (copy-list *physics-event-list*))

;; @TODO
;; (defun velocity ((event physics-event))
;; something using prolog queries just like in cram-projection-demos/src/utilities/objects.lisp

(defmethod single-check ((event physics-event))
  "This function will check for occurance of event based on serveral conditions, and return nil if it should not be checked again, t otherwise"
  (if (removal-requested event)
    (setf (run-status event) nil)
    (progn ;(ros-info EVENT_BULLET_WORLD "Checking one single time")
      (let ((detail-status (funcall (raise-event-on-true event) event)))
            ; use the constraint_relation here to set the final status
            (if (equal detail-status t) (setf (status event) t)))
      (if (status event)
        (progn 
          (setf (message event) (prepare-msg event))
          (on-event event)))
      t)))

(defmethod create-thread ((event physics-event))
  ;; @TODO: event specific mutex. Is it required??
  "Common function to create thread for some event. Calling it twice for same event would result in error since 2 threads with same name will be created"
  (make-thread :name (concatenate 'string (event-name event) "-thread")
    (thread-function event))
)

(defmethod thread-function ((event physics-event))
  "Function to be called inside a thread. Seperate to facilitate return-from"
    (ros-info EVENT_BULLET_WORLD "Creating thread for ~a event ~%" (event-name event))
    (loop-at-most-every (get-param "loop-rate")
      (if (run-status event) (single-check event) (return-from thread-function nil)))
    (ros-info EVENT_BULLET_WORLD "Exiting thread for ~a event ~%" (event-name event))
)

(defmethod prepare-msg ((event physics-event))
  "Creates messages to be published with results of constraints, their violations, etc."
  (make-message (get-ros-name "EventUpdate")
                :header (make-msg "std_msgs/Header"
                                  :stamp (ros-time)
                                  :seq (length (occurance-stack event))
;                                  :frame_id (get-node-name (get-ros-name *current-bullet-world* ))
                                  )
                :name (event-name event)
                :number_of_constraints (length (constraints event))
                :status (make-array (length (constraints event)) :initial-contents (loop for x in (constraints event) collect (bool-to-num x)))
                :has_occured (bool-to-num (status event)))
)
; end class physics-event related stuff
