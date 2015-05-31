# Message for CRAM Event raising
# header for timestamp, serial no. etc
std_msgs/Header header

# meta-data about the event
string event_name
int32 ros_binding_type

# constraints to be applied under the given name
int32 is_custom
int32 number_of_constraints
event_bullet_world/Constraint[] constraints
string custom_function

# ros_binding_type values
int32 NONE      = 0
int32 MESSAGE   = 1
int32 SERVICE   = 2
int32 ACTION    = 3
int32 PARAMETER = 4
