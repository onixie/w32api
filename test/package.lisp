(defpackage #:w32api/test
  (:use #:cl #:cffi #:fiveam #:w32api #:bordeaux-threads))

(in-package #:w32api/test)

(def-suite test)

(defvar *run-test-silently* t)
