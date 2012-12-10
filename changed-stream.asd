(defsystem :changed-stream
  :name "changed-stream"
  :version "1.0.0"
  :author "Warren Wilkinson <warrenwilkinson@gmail.com>"
  :description "A stream upon another stream that adds a change."
  :components ((:file "changed-stream"))
  :in-order-to ((test-op (load-op changed-stream.tests))))
  
(defmethod perform ((op asdf:test-op) (system (eql (find-system :changed-stream))))
  (funcall (intern "RUN-TESTS" :changed-stream.test)))
