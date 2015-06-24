# Message for generic constraints

# If any variable (which takes options from the bottom
# of this message) is out of range, the message will be dropped

# If empty, target_object will be assumed to be the world itself
string target_object
string source_object
int32 constraint_type

# Linear or angular?
int32 is_angular
# Min and max in case of a range.
# For a particular value val, set min to val-delta, max to val+delta
geometry_msgs/Vector3 min
geometry_msgs/Vector3 max

# Event raised inside/outside the range
int32 is_interior
# Is the constraint a vector or a scalar?
int32 is_scalar
# Is the data for source relative to the target_object? Doesn't matter if the target_object is empty
int32 is_relative

# constraint_type values
int32 POSITION = 0
int32 VELOCITY = 1
int32 ACCELERATION = 2

# is_angular, is_interior, is_relative values
int32 TRUE = 1
int32 FALSE = 0

# some difficult to express range limits
# float64 DOUBLE_INF = 1.7976931348623157e+308
float32 DOUBLE_INF = 340282346638528859811704183484516925440  # lisp has single precision float only
float32 DOUBLE_DELTA = 1.0e-5
int32 INT_INF = 2147483647
int32 INT_DELTA = 0
