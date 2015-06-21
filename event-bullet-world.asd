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
               )
  :components
  ((:module "src"
            :components
            ((:file "package")
             (:module "world-event" :depends-on ("package")
                      :components
                      (
                       
                       (:file "ros-bindings")
                       (:file "node" :depends-on ("ros-bindings"))))
             ))))
