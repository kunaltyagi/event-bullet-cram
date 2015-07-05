(defsystem event-bullet-world
  :author "Kunal Tyagi"
  :license "BSD"
  :depends-on (cram-language
               cram-reasoning
               bullet-reasoning
               cram-environment-representation
               cram-plan-knowledge
               bullet-reasoning-designators
               roslisp
               std_msgs-msg
               geometry_msgs-msg
               event_bullet_world-srv
               event_bullet_world-msg
               )
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:file "object-event" :depends-on ("world-event" "package"))
             (:module "world-event" :depends-on ("package")
                      :components
                      (
                       (:file "physics-event")
                       (:file "ros-bindings" :depends-on ("physics-event"))
                       (:file "node" :depends-on ("ros-bindings"))
                      ))
             ))))
