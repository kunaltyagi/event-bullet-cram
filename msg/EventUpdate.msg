# Message for CRAM Event raising
# header for timestamp, serial no. etc (of the
# event if it occured)
std_msgs/Header header

# event name
string name
# event status
int32 status

# @TODO
#int32 number_of_constraints
#event_bullet_world/Constraint[] constraint_status
