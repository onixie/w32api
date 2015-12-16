(defpackage #:w32api.kernel32
  (:use #:common-lisp #:cffi #:w32api.type)
  (:export GetModuleHandleW
	   GetLastError
	   FormatMessageW
	   GetCurrentThreadId
	   GetCurrentProcessId
	   GetCurrentThread
	   GetCurrentProcess
	   GetCurrentProcessorNumber
	   GetCommandLineW
	   GetFirmwareType
	   GetVersionExW
	   GetSystemInfo
	   GetNativeSystemInfo
	   GetComputerNameExW
	   GetProductInfo
	   IsProcessorFeaturePresent
	   IsNativeVhdBoot
	   GetSystemDirectoryW
	   GetWindowsDirectoryW
	   GetSystemWindowsDirectoryW
	   CreateToolhelp32Snapshot
	   Thread32First
	   Thread32Next
	   Process32First
	   Process32Next
	   Module32First
	   Module32Next
	   Heap32First
	   Heap32Next
	   Heap32ListFirst
	   Heap32ListNext
	   CloseHandle))

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

(defcfun "GetCurrentThread" HANDLE)

(defcfun "GetCurrentProcessId" DWORD)

(defcfun "GetCurrentProcess" HANDLE)

(defcfun "GetCurrentProcessorNumber" DWORD)

(defcfun "GetCommandLineW" :string)

(defcfun "GetNativeSystemInfo" :void
  (lpSystemInfo (:pointer (:struct SYSTEM_INFO))))

(defcfun "GetSystemInfo" :void
  (lpSystemInfo (:pointer (:struct SYSTEM_INFO))))

;;; Win 8 or later
(defcfun "GetFirmwareType" :boolean
  (FirmwareType (:pointer FIRMWARE_TYPE_ENUM)))

(defcfun "GetVersionExW" :boolean
  (lpVersionInfo (:pointer (:struct OSVERSIONINFOEX))))

;;; Win Vista or later
(defcfun "GetProductInfo" :boolean
  (dwOSMajorVersion  DWORD)
  (dwOSMinorVersion  DWORD)
  (dwSpMajorVersion  DWORD)
  (dwSpMinorVersion  DWORD)
  (pdwReturnedProductType (:pointer PRODUCT_ENUM)))

(defcfun "GetComputerNameExW" :boolean
  (NameType COMPUTER_NAME_FORMAT_ENUM)
  (lpBuffer :string)
  (lpnSize (:pointer DWORD)))

(defcfun "IsProcessorFeaturePresent" :boolean
  (ProcessorFeature PF_ENUM))

;;; Win 8 or later
(defcfun "IsNativeVhdBoot"  :boolean
  (NativeVhdBoot (:pointer :boolean)))

(defcfun "GetSystemWindowsDirectoryW" :uint
  (lpBuffer :string)
  (uSize   :uint))

(defcfun "GetWindowsDirectoryW" :uint
  (lpBuffer :string)
  (uSize   :uint))

(defcfun "GetSystemDirectoryW" :uint
  (lpBuffer :string)
  (uSize   :uint))

(defcfun "CreateToolhelp32Snapshot" HANDLE
  (dwFlags TH32CS_FLAG)
  (th32ProcessID DWORD))

(defcfun "Thread32First" :boolean
  (hSnapshot HANDLE)
  (lpte (:pointer (:struct THREADENTRY32))))

(defcfun "Thread32Next" :boolean
  (hSnapshot HANDLE)
  (lpte (:pointer (:struct THREADENTRY32))))

(defcfun ("Process32FirstW" Process32First) :boolean
  (hSnapshot HANDLE)
  (lppe (:pointer (:struct PROCESSENTRY32))))

(defcfun ("Process32NextW" Process32Next) :boolean
  (hSnapshot HANDLE)
  (lppe (:pointer (:struct PROCESSENTRY32))))

(defcfun ("Module32FirstW" Module32First) :boolean
  (hSnapshot HANDLE)
  (lpme (:pointer (:struct MODULEENTRY32))))

(defcfun ("Module32NextW" Module32Next) :boolean
  (hSnapshot HANDLE)
  (lpme (:pointer (:struct MODULEENTRY32))))

(defcfun "Heap32First" :boolean
  (lphe (:pointer (:struct HEAPENTRY32)))
  (th32ProcessID DWORD)
  (th32HeapID ULONG_PTR))

(defcfun "Heap32Next" :boolean
  (lphe (:pointer (:struct HEAPENTRY32))))

(defcfun "Heap32ListFirst" :boolean
  (hSnapshot HANDLE)
  (lphl (:pointer (:struct HEAPLIST32))))

(defcfun "Heap32ListNext" :boolean
  (hSnapshot HANDLE)
  (lphl (:pointer (:struct HEAPLIST32))))

(defcfun "CloseHandle" :boolean
  (hObject HANDLE))
