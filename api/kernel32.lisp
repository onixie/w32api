(defpackage #:w32api.kernel32
  (:use #:common-lisp #:cffi #:w32api.type)
  (:export GetModuleHandleW
	   GetLastError
	   FormatMessageW))

(in-package #:w32api.kernel32)

(define-foreign-library kernel32
  (:win32 "kernel32.dll"))

(use-foreign-library kernel32)

(defcfun "GetModuleHandleW" HMODULE
  (lpModuleName :string))

(defcfun "GetLastError" DWORD)

(defcfun "FormatMessageW" DWORD
  (dwFlags   DWORD)
  (lpSource (:pointer :void))
  (dwMessageId   DWORD)
  (dwLanguageId   DWORD)
  (lpBuffer  :string)
  (nSize   DWORD)
  (va_Arguments (:pointer :pointer)))

