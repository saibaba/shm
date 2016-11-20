;;;; mem.lisp
(defpackage #:mem
  (:use :common-lisp :cffi)
  (:export #:write-to-pointer #:read-from-pointer))

(in-package #:mem)

(defun write-to-pointer (ptr val)
  (setf (mem-ref ptr :long) val))

(defun read-from-pointer (ptr)
  (mem-ref ptr :long 0))

