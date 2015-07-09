(in-package :events)

(defun MultiArrayDimension (&key (label " ") (size 0) (stride 0))  ; label is string, size is integer, stride is integer
  "Returns a message of type MultiArrayDimension"
  (make-msg "std_msgs/MultiArrayDimension" :size size :label label :stride stride))

(defun Int32MultiArray (&key (data '()) (size '()))  ; data and size are lists
  "Creates a Int32MultiArray message with data and layout filled with required messages formed using size"
  ; in layout slot, label and stride are untouched
  (make-message "std_msgs/Int32MultiArray"
                :data (make-array (length data) :initial-contents data)
                :layout (make-message "std_msgs/MultiArrayLayout"
                                      :dim (make-array (length size)
                                                       :initial-contents (mapcar '#(lambda (num)
                                                                                     (MultiArrayDimension :size num))
                                                                                 size)))))
