(defpackage #:w32api.kernel32
  (:use #:common-lisp #:cffi #:w32api.type)
  (:export GetModuleHandleA
	   GetLastError
	   FormatMessageA))

(in-package #:w32api.kernel32)

(define-foreign-library kernel32
  (:win32 "kernel32.dll"))

(use-foreign-library kernel32)

(defcfun "GetModuleHandleA" HMODULE
  (lpModuleName :string))

(defcfun "GetLastError" DWORD)

(defcfun "FormatMessageA" DWORD
  (dwFlags   DWORD)
  (lpSource (:pointer :void))
  (dwMessageId   DWORD)
  (dwLanguageId   DWORD)
  (lpBuffer  :string)
  (nSize   DWORD)
  (va_Arguments (:pointer :pointer)))
