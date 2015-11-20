(defpackage #:w32api.gdi32
  (:use #:common-lisp #:cffi #:w32api.type)
  (:export TextOutW))

(in-package #:w32api.gdi32)

(define-foreign-library gdi32
  (:win32 "gdi32.dll"))

(use-foreign-library gdi32)

(defcfun "TextOutW" :boolean
  (hdc HDC)
  (nXStart :int)
  (nYStart :int)
  (lpString :string)
  (cchString :int))
