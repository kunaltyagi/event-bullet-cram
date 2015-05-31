# Message for CRAM Event raising
# header for timestamp, serial no. etc (of the
# event if it occured)
std_msgs/Header header

# event name
string name
# event status
int32 has_occured
# detailed status
int32 number_of_constraints
int32[] status

int32 TRUE = 1
int32 FALSE = 0

