(defpackage #:w32api.kernel32
  (:use #:common-lisp #:cffi #:w32api.type)
  (:export GetModuleHandleW
	   GetLastError
	   FormatMessageW
	   OpenThread
	   TerminateThread
	   ResumeThread
	   SuspendThread
	   ExitThread
	   GetExitCodeThread
	   OpenProcess
	   TerminateProcess
	   ExitProcess
	   GetExitCodeProcess
	   GetCurrentProcessorNumber
	   GetActiveProcessorCount
	   GetActiveProcessorGroupCount
	   GetMaximumProcessorCount
	   GetMaximumProcessorGroupCount
	   GetCurrentThreadId
	   GetCurrentProcessId
	   GetCurrentThread
	   GetCurrentProcess
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
	   Process32FirstW
	   Process32NextW
	   Module32FirstW
	   Module32NextW
	   Heap32First
	   Heap32Next
	   Heap32ListFirst
	   Heap32ListNext
	   CloseHandle
	   WaitForSingleObjectEx
	   WaitForMultipleObjectsEx))

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

(defcfun "Toolhelp32ReadProcessMemory" :boolean
  (th32ProcessID DWORD)
  (lpBaseAddress (:pointer :void))
  (lpBuffer (:pointer :void))
  (cbRead SIZE_T)
  (lpNumberOfBytesRead SIZE_T))

(defcfun "Thread32First" :boolean
  (hSnapshot HANDLE)
  (lpte (:pointer (:struct THREADENTRY32))))

(defcfun "Thread32Next" :boolean
  (hSnapshot HANDLE)
  (lpte (:pointer (:struct THREADENTRY32))))

(defcfun "Process32FirstW" :boolean
  (hSnapshot HANDLE)
  (lppe (:pointer (:struct PROCESSENTRY32))))

(defcfun "Process32NextW" :boolean
  (hSnapshot HANDLE)
  (lppe (:pointer (:struct PROCESSENTRY32))))

(defcfun "Module32FirstW" :boolean
  (hSnapshot HANDLE)
  (lpme (:pointer (:struct MODULEENTRY32))))

(defcfun "Module32NextW" :boolean
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

;;; Thread
(defcfun "OpenThread" HANDLE
  (dwDesiredAccess THREAD_FLAG)
  (bInheritHandle  :boolean)
  (dwThreadId DWORD))

(defcfun "TerminateThread" :boolean
  (hThread HANDLE)
  (dwExitCode  DWORD))

(defcfun "ResumeThread" DWORD
  (hThread HANDLE))

(defcfun "SuspendThread" DWORD
  (hThread HANDLE))

(defcfun "ExitThread" :void
  (dwExitCode DWORD))

(defcfun "GetExitCodeThread" :boolean
  (hThread HANDLE)
  (lpExitCode (:pointer DWORD)))

(defcfun "GetCurrentThreadId" DWORD)

(defcfun "GetCurrentThread" HANDLE)

;;; Process
(defcfun "OpenProcess" HANDLE
  (dwDesiredAccess PROCESS_FLAG)
  (bInheritHandle  :boolean)
  (dwProcessId DWORD))

(defcfun "TerminateProcess" DWORD
  (hProcess HANDLE)
  (uExitCode :uint))

(defcfun "ExitProcess" :void
  (uExitCode :uint))

(defcfun "GetExitCodeProcess" :boolean
  (hProcess HANDLE)
  (lpExitCode (:pointer DWORD)))

(defcfun "GetCurrentProcessId" DWORD)

(defcfun "GetCurrentProcess" HANDLE)

;;; Processor
(defcfun "GetActiveProcessorCount" DWORD
  (GroupNumber WORD))

(defcfun "GetActiveProcessorGroupCount" DWORD)

(defcfun "GetCurrentProcessorNumber" DWORD)

(defcfun "GetMaximumProcessorCount" DWORD
  (GroupNumber WORD))

(defcfun "GetMaximumProcessorGroupCount" DWORD)

;;; Synchronization
(defcfun "WaitForSingleObjectEx" WAIT_RESULT_ENUM
  (hHandle HANDLE)
  (dwMilliseconds DWORD)
  (bAlertable :boolean))

(defcfun "WaitForMultipleObjectsEx" WAIT_RESULT_ENUM
  (nCount  DWORD)
  (lpHandles (:pointer HANDLE))
  (bWaitAll :boolean)
  (dwMilliseconds DWORD)
  (bAlertable :boolean))
