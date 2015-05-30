# event-bullet-cram
Event raising platform for CRAM using bullet

## Aim
This package is aimed at creating a common platform for objects in a bullet-world to raise events if certain criteria is met. The events raised by the objects, without prior knowledge to this platform are also supported, and they are passed onto such that the end user has no knowledge if the event was raised by this platform or from outside it.

Each event is differentiated by its name, its creation time. Right now, creation time is not given much importance (actually, given none). THis behavior can change in near future.

## Description
Event is described as a named violation of a list of constraints. A constraint is a simple check on relative or absolute value (magnitude or direction (or both?? haven't given both much thought)) of position, velocity or accelration (linear or angular) of a source (wrt a target object) with repsect to a range.
