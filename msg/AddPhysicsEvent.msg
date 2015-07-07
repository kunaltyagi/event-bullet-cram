# Message for CRAM Event raising
# header for timestamp, serial no. etc
std_msgs/Header header

# meta-data about the event
string event_name
int32 ros_binding_type

# constraints to be applied under the given name
int32 is_custom
# used if is_custom is false. NONE to neglect constraint_b
int32 number_of_constraints  # required only for C++

## DEPRECIATED
int32[] constraint_relation  # of length (number_of_constraints-1)
## END DEPRECIATED

std_msgs/Int32MultiArray boolean_expression
# It will contain data like:
# boolean_expression.
#     layout.dim[0].size = 2
#     layout.dim[1].size = 4
#     layout.dim[2].size = 1
#     data = [2,3,1,2,5,3,2]
# This translates into SOP of: (BC + ABCE + D)
#                  and POS of: (B+C)(A+B+C+E)(D)
# where the no. of elements in a group is denoted by the dim[group_no].size
# constraint_list = [A, B, C, D, E], ie,
# A is 1st element, and denoted by 1 in the boolean_expression.data

int32 is_product_of_sums  # if false, it is taken to be sum_of_products
# Hint: SOP determines which are active, POS determines which should be inactive (crude sense)

event_bullet_world/PhysicsConstraint[] constraint_list
# custom function. All details of this message is available through variable
# 'event' of class physics-event in lisp. This should contain only the body of the
# function, ie. provide only ellipsis in (defun <fn-name> (event) ( ... ))
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
