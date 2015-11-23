(defpackage #:w32api.kernel32
  (:use #:common-lisp #:cffi #:w32api.type)
  (:export GetModuleHandleW
	   GetLastError
	   FormatMessageW
	   GetCurrentThreadId
	   GetCurrentProcessId
	   GetCurrentProcessorNumber
	   GetCommandLineW
	   GetFirmwareType
	   GetVersionExW
	   GetSystemInfo
	   GetNativeSystemInfo
	   GetComputerNameExW
	   GetProductInfo))

(in-package #:w32api.kernel32)

(define-foreign-library kernel32
  (:win32 "kernel32.dll"))

(use-foreign-library "kernel32")

(defcfun "GetModuleHandleW" HMODULE
  (lpModuleName :string))

(defcfun "GetLastError" DWORD)

(defcfun "FormatMessageW" DWORD
  (dwFlags   FORMAT_MESSAGE_FLAG)
  (lpSource (:pointer :void))
  (dwMessageId   DWORD)
  (dwLanguageId   DWORD)
  (lpBuffer  :string)
  (nSize   DWORD)
  (va_Arguments (:pointer :pointer)))

(defcfun "GetCurrentThreadId" DWORD)

(defcfun "GetCurrentProcessId" DWORD)

(defcfun "GetCurrentProcessorNumber" DWORD)

(defcfun "GetCommandLineW" :string)

(defcfun "GetNativeSystemInfo" :void
  (lpSystemInfo (:pointer (:struct SYSTEM_INFO))))

(defcfun "GetSystemInfo" :void
  (lpSystemInfo (:pointer (:struct SYSTEM_INFO))))

(defcfun "GetFirmwareType" :boolean
  (FirmwareType (:pointer FIRMWARE_TYPE_ENUM)))

(defcfun "GetVersionExW" :boolean
  (lpVersionInfo (:pointer (:struct OSVERSIONINFOEX))))

(defcfun "GetProductInfo" :boolean;;; Winver > 6.0
  (dwOSMajorVersion  DWORD)
  (dwOSMinorVersion  DWORD)
  (dwSpMajorVersion  DWORD)
  (dwSpMinorVersion  DWORD)
  (pdwReturnedProductType (:pointer PRODUCT_ENUM)))

(defcfun "GetComputerNameExW" :boolean
  (NameType COMPUTER_NAME_FORMAT_ENUM)
  (lpBuffer :string)
  (lpnSize (:pointer DWORD)))
