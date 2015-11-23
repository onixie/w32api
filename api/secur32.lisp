(defpackage #:w32api.secur32
  (:use #:common-lisp #:cffi #:w32api.type)
  (:export GetUserNameExW))

(in-package #:w32api.secur32)

(define-foreign-library secur32
  (:win32 "secur32.dll"))

(use-foreign-library "secur32")

(defcfun "GetUserNameExW" :boolean
  (NameFormat EXTENDED_NAME_FORMAT_ENUM)
  (lpNameBuffer :string)
  (lpnSize (:pointer :ulong)))
