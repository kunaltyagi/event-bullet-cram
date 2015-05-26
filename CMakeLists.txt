cmake_minimum_required(VERSION 2.8.3)
project(event_bullet_world)
find_package(catkin REQUIRED COMPONENTS
  message_generation std_msgs geometry_msgs
  sensor_msgs actionlib actionlib_msgs
  cram_language
)

add_message_files(DIRECTORY msg    FILES AddEvent.msg Constraint.msg)
add_service_files(DIRECTORY srv    FILES EventStatus.srv)

generate_messages(DEPENDENCIES std_msgs geometry_msgs actionlib_msgs)

catkin_package(CATKIN_DEPENDS actionlib actionlib_msgs roslisp std_msgs message_runtime)
