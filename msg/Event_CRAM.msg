# Message for CRAM Event raising
Header header

string event_name
string object_name
string object_type

int32 NO_CONSTRAINT = 0
int32 POSITION_CONSTRAINT = 1
int32 VELOCITY_CONSTRAINT = 2
int32 ACCELERATION_CONSTRAINT = 3
int32 OTHER_CONSTRAINT = 4
int32 constraint

int32 NO_EVENT = 0
int32 CONSTRAINT_BREACH_EVENT = 1
int32 value
