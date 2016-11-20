;;;; shm.asd

(asdf:defsystem #:shm
  :description "Describe shm here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (:cffi)
  :components (
    (:file "mem")
    (:file "syscalls")
    (:file "shm")))

