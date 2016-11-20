;;;; shm.lisp
(defpackage #:shm
  (:use :common-lisp :mem :syscalls :cffi))

(in-package #:shm)

(defun sample()
  (let* (
         (shm-name "sample")
         (dtls (unix-shm-open-and-map shm-name))
         (ptr (first dtls))
         (region_size (second dtls)))
    (when (zerop (unix-fork))
      (child-work ptr))
    (parent-work ptr)
    (unix-munmap ptr region_size)
    (unix-shm-unlink shm-name)))

(defun child-work (ptr)
  (format t "I'm child, ptr=~A Pid=~A~%...writing to shared memory~%" ptr (unix-getpid))
  (write-to-pointer ptr #xdbeebee)
  (format t "child says child wrote ~x~%" (read-from-pointer ptr))
  (unix-exit 0))

(defun parent-work(ptr)
  (progn
    (format t "I'm parent, Pid=~A~%" (unix-getpid))
    (format t "one child claimed!~A~%" (unix-wait (cffi:null-pointer)))
    (format t "parent says child wrote ~x~%" (read-from-pointer ptr))))
