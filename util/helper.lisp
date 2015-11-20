(in-package #:w32api.util)

(defun ensure-list (thing)
  (if (listp thing)
      thing
      (list thing)))
