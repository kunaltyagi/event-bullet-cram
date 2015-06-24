(in-package :cl-user)

(defpackage event-bullet-world
  (:nicknames #:events)
  (:use #:cram-roslisp-common
        #:common-lisp
        #:cpl-impl
        #:cram-plan-knowledge
        #:cram-reasoning
        #:roslisp
        #:cl
        "CL" "SB-THREAD" "SB-EXT")
  (:shadowing-import-from #:cpl #:name #:fail #:wait-for)
  (:shadowing-import-from #:btr object pose object-pose)
  (:export check-map)
  )

