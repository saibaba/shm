I References

https://www.ibm.com/developerworks/aix/library/au-spunix_sharedmemory/
http://xach.livejournal.com/278047.html

II Setup


1) installed quicklisp.lisp so everytime lisp is started after this ql:quickload is already loaded
2) created ~/.config/common-lisp/source-registry.conf.d/projects.conf 
3) ran (quickproject:make-project "shm"). Essentially did this...
   a) * (ql:quickload "quickproject")
   b) * (quickproject:make-project "shm")


III Usage

run tests.sh
