# Message for generic constraints

# If any variable (which takes options from the bottom
# of this message) is out of range, the message
# will be dropped

# If empty, target_object will be assumed to be the world itself
string target_object
string source_object
int32 constraint_type

# Constraint is a range or a single point
int32 is_not_range
# Linear or angular?
int32 is_angular
# Data to be used if not_range is true
geometry_msgs/Vector3 data
# Min and max in case of a range
geometry_msgs/Vector3 min
geometry_msgs/Vector3 max

# Event raised inside/outside the range
int32 is_interior
# Is it a direction constraint or a magnitude constraint?
int32 is_magnitude_constraint
# Is the data for source relative to the target_object?
# Doesn't matter if the target_object is empty
int32 is_relative

# constraint_type values
int32 POSITION = 0
int32 VELOCITY = 1
int32 ACCELERATION = 2

# is_not_range, is_angular, is_interior, is_relative values
int32 TRUE = 1
int32 FALSE = 0
