(defsystem :asc
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3) (safety 3) (speed 0)))
                    (funcall next))
  :components ((:module "source"
                        :pathname "./"
                        :components ((:file "package")
                                     (:file "support.asc" :depends-on ("package"))
                                     (:file "helloworld.asc" :depends-on ("support.asc"))
                                     (:file "helloworld5.asc" :depends-on ("support.asc"))
                                     (:file "helloworld6.asc" :depends-on ("support.asc"))
                                     (:file "test.asc" :depends-on ("helloworld.asc"
								    "helloworld5.asc"
								    "helloworld6.asc"))))))

