# Message for CRAM Event raising
# header for timestamp, serial no. etc
std_msgs/Header header

# meta-data about the event
string event_name
int32 ros_binding_type

# constraints to be applied under the given name
int32 is_custom
# used if is_custom is false. NONE to neglect constraint_b
# constraint_relation[i] is for constraint_list[i] and constraint_list[i+1]
int32 number_of_constraints
int32[] constraint_relation  # of length (number_of_constraints-1)

event_bullet_world/PhysicsConstraint[] constraint_list
# custom function. not implemented yet.
string custom_function

int32 NONE      = 0

# ros_binding_type values
int32 MESSAGE   = 1
int32 SERVICE   = 2
int32 ACTION    = 3  # not available
int32 PARAMETER = 4

# constraint_type values
int32 AND = 1
int32 OR = 2
int32 XOR = 3
int32 NOT = 4

# is_custom values
int32 TRUE = 1
int32 FALSE = 0
