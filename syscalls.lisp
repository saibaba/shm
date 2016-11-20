;;;; syscalls.lisp
(defpackage #:syscalls
    (:use :common-lisp :cffi)
    (:export
      #:enum-sysconf-type #:enum-open-flag #:enum-mmapn-flag
      #:open-flag #:sysconf-type #:prot-flag #:mmap-flag
      #:unix-getpid #:unix-getuid #:unix-getgid #:unix-fork
      #:unix-exit #:unix-wait #:unix-perror
      #:unix-shm-open-and-map #:unix-shm-unlink #:unix-munmap
      #:unix-sysconf #:unix-ftruncate #:unix-perror #:unix-mmap))

(in-package #:syscalls)

(asdf:load-system :cffi)

(defcvar "errno" :int)
(cffi:defcfun ("getpid" unix-getpid) :int)
(cffi:defcfun ("getuid" unix-getuid) :int)
(cffi:defcfun ("getgid" unix-getgid) :int)
(cffi:defcfun ("fork" unix-fork) :int)
(cffi:defcfun ("exit" unix-exit) :void (code :int))
(cffi:defcfun ("wait" unix-wait) :int (statloc :pointer))
(cffi:defcfun ("sysconf" unix-sysconf) :int (name :long))
(cffi:defcfun ("ftruncate" unix-ftruncate) :int (filedes :int) (leng :int))
(cffi:defcfun ("mmap" unix-mmap) :pointer (addr :pointer) (len :int) (prot :int) (flags :int) (fd :int) (offset :int))
(cffi:defcfun ("munmap" unix-munmap) :int (ptr :pointer) (len :int))

(cffi:defcenum enum-sysconf-type
  (:_SC_PAGE_SIZE 29))

(cffi:defcenum enum-open-flag
  (:O_CREAT #x200)
  (:O_TRUNC #x400)
  (:O_RDWR  #x002))

(cffi:defcenum enum-prot-flag
  (:PROT_READ #x01)
  (:PROT_WRITE #x02))

(cffi:defcenum enum-mmap-flag
  (:MAP_SHARED #x0001))

(defun open-flag (flg)
  (cffi:foreign-enum-value 'enum-open-flag flg))

(defun sysconf-type (flg)
  (cffi:foreign-enum-value 'enum-sysconf-type flg))

(defun prot-flag (flg)
  (cffi:foreign-enum-value 'enum-prot-flag flg))

(defun mmap-flag (flg)
  (cffi:foreign-enum-value 'enum-mmap-flag flg))

(defun unix-perror (label)
  (with-foreign-pointer-as-string (str 255)
    (lisp-string-to-foreign label str (+ 1 (length label)))
    (cffi:foreign-funcall "perror" :string str :void)))

(defun unix-shm-open-and-map (name)
  (with-foreign-pointer (str 30)
    (lisp-string-to-foreign name str (+ 1 (length name)) :encoding :ascii)
    (let* ((fd (cffi:foreign-funcall "shm_open" :pointer str :int (logior (open-flag :O_CREAT) (open-flag :O_RDWR)) :int #o666 :int))
           (region_size (unix-sysconf (sysconf-type :_SC_PAGE_SIZE)))
           (r (unix-ftruncate fd region_size))
           ;(d (unix-perror "ftruncate"))
           (ptr (unix-mmap (null-pointer) region_size (logior (prot-flag :PROT_READ) (prot-flag :PROT_WRITE)) (mmap-flag :MAP_SHARED) fd 0)))
      (format t "open fd=~A rgn-sz=~A r=~A ptr=~A~%" fd region_size r ptr)
      (list ptr region_size))))

(defun unix-shm-open-and-map2 (name)
  (with-foreign-pointer-as-string (str 30)
    (progn
      (lisp-string-to-foreign name str (+ 1 (length name)) :encoding :ascii)
      (let* ((fd (cffi:foreign-funcall "shm_open" :pointer str :int (logior (open-flag :O_CREAT) (open-flag :O_RDWR)) :int #o666 :int))
             (region_size (unix-sysconf (sysconf-type :_SC_PAGE_SIZE)))
             (r (unix-ftruncate fd region_size))
             ;(d (unix-perror "ftruncate"))
             (ptr (unix-mmap (null-pointer) region_size (logior (prot-flag :PROT_READ) (prot-flag :PROT_WRITE)) (mmap-flag :MAP_SHARED) fd 0)))
        (format t "open fd=~A rgn-sz=~A r=~A ptr=~A~%" fd region_size r ptr)
        ptr))))

(defun unix-shm-unlink (name)
  (with-foreign-pointer-as-string (str 30)
    (lisp-string-to-foreign name str (+ 1 (length name)) :encoding :ascii)
    (cffi:foreign-funcall "shm_unlink" :pointer str :int)))
