cmake_minimum_required(VERSION 2.8.3)
project(event_bullet_world)
find_package(catkin REQUIRED COMPONENTS
  cram_language
)

add_message_files(DIRECTORY msg    FILES Event_CRAM.msg)
add_service_files(DIRECTORY srv    FILES Event_CRAM.msg)
add_action_files (DIRECTORY action FILES Event_CRAM.msg)

generate_messages(DEPENDENCIES actionlib_msgs std_msgs)

catkin_package(CATKIN_DEPENDS actionlib actionlib_msgs roslisp std_msgs message_runtime)
