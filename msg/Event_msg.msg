# Message for CRAM Event raising
# header for timestamp, serial no. etc
std_msgs/Header header

# meta-data about the event
string event_name

# constraints to be applied under the given name
int32 number_of_constraints
event_bullet_world/Constraint[] constraints
