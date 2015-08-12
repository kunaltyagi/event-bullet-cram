# Developer's Guide

## Organization of code
### root folder
* **event-bullet-world.asd** file for building
* **CMakeLists.txt** is very basic and might need to be changed, especially if one wishes to create binaries by running ```roscd event_bullet_world; cd ../..; catkin_make --pkg event_bullet_world```
* **README.md** contains basic information
* **reasoning.md** (to be added) contains most of my thought process and reasoning while developing this package
* **developer_guide.md**: This file itself
 
### msg
Contains 3  messages:
* **AddPhysicsEvent**: Used to add an event by publishing on the topic "[node_name]/physics/add_event"
* **EventUpdate**: Updates for all events are of this type, and can be accessed on the topic "[node_name]/event_update"
* **PhysicsConstraint**: This is used to build the AddPhysicsEvent message

### srv
Contains 1 service:
* **EventStatus**: Sends the latest event status in an EventUpdate message for the provided event name. This functionality is provided for applications which require a polling method of updates and is accessible at "[node_name]/event_status"

### src
*__TODO__: A lot of code is repeated and not properly written because of my inexperience with CLOS objects*
* **EventStatus**: This service provides a way for polling based algorithms to work. The service is accessed by name [node_name]/event_status
* **package.lisp** is a standard lisp file which make it easier to use a lot of functions from cram and roslisp namespaces
* **object-event.lisp**: This file contains code which enables the objects in a bullet map to create and broadcast events via lisp code without the event being added by the user (through the AddPhysicsEvent message)
* **helper.lisp** is file which contains code used to test code in other lisp files and was mainly used in integration.
* **world-event** folder contains most of the actual code of this repository:
	* **node.lisp** is the ROS node interface which enables the code to be ported to the user's wishes quite easily to a different type of ROS node or with other features apart from just the existing ones.
    check-map is the only function which should be accessed by the user to run this node. It accepts two arguments [as evident here](https://github.com/kunaltyagi/event_bullet_world/blob/master/src/world-event/node.lisp#L3).
    * **ros-bindings.lisp**: Some of the code in this file should actually be in the next file (and vice-versa). This file essentially provides a wrapper around ROS related functionalities like subscribers, publishers, service servers, etc. and also creates messages and services and required CLOS objects from them.
    * **physics-event.lisp**: This contains code pertaining to events based on checking of physics constraints, running checks on them as required in a loop, etc. apart from other supplementary code required, some of which can be shifted else where.
