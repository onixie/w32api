(in-package #:w32api.util)

(defun ensure-list (thing)
  (if (listp thing)
      thing
      (list thing)))

(defun make-keyword (name)
  (values (intern (substitute #\- #\space (string-upcase name)) :keyword)))
