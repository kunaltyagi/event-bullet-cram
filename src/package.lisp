(in-package :cl-user)

(defpackage event-bullet-world
  (:nicknames #:events)
  (:use #:cram-roslisp-common
        #:common-lisp
        #:cpl-impl
        #:cram-plan-knowledge
        #:cram-reasoning
        #:roslisp)
  (:shadowing-import-from #:cpl #:name #:fail)
  (:shadowing-import-from #:btr object pose object-pose))

