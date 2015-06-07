(defsystem event-bullet-world
  :author "Kunal Tyagi"
  :license "BSD"
  :depends-on (cram-language
               cram-reasoning
               bullet-reasoning
               cram-environment-representation
               cram-plan-knowledge
               bullet-reasoning-designators
               roslisp)
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:module "world-event" :depends-on ("package")
                      :components
                      ((:file "world-events")
                       (:file "event-list" :depends-on ("world-events"))
                       (:file "ros-bindings" :depends-on ("event-list"))
                       (:file "node" :depends-on ("ros-bindings"))))
             (:file "object-event" :depends-on ("package" "world-event"))))))
