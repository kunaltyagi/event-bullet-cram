# event-bullet-cram
Event raising platform for CRAM using bullet

## Aim
This package is aimed at creating a common platform in a bullet-world to raise events if certain criteria is met. The events raised by the objects, without prior knowledge to this platform are also supported, and they are passed onto such that the end user has no knowledge if the event was raised by this platform or from outside it.

**NOTE**: Each event is differentiated by its name only. Right now, creation time is not given much importance (actually, given none). This behavior can change in near future.

## Description
Event is described as a named violation of a list of constraints. A constraint is a simple check on relative or absolute value (magnitude or direction) of position, velocity or accelration (linear or angular) of a source (wrt a target object) with repsect to a range. The constraints are combined based on Sum-of-Proucts or Product-of-Sums to qualify as an event.

## Usage
* **$** roslisp_repl
* **CL_USER>** ```(ros-load:load-system "event_bullet_world" "event-bullet-world")```
* **CL_USER>** ```(in-package :events)```
* **EVENTS>** ```(check-map :node-name <name> :loop-rate <rate in Hz>)```

It provides a topic to add events <TOPIC_NAME>, a topic for async feedback and a service for sync feedback. 

## In Pipeline
Topics for query such as object-generated events, existing events, details of events are to be added. Also, the constraints have not been added as of 8 July 2015
