(defpackage #:w32api.user32
  (:use #:common-lisp #:cffi #:w32api.type)
  (:export RegisterClassExA
	   UnregisterClassA
	   GetClassNameA

;;	   CreateWindowA
	   CreateWindowExA
	   SetParent
	   SetWindowLongPtr
	   AnimateWindow
	   EnableWindow
	   IsWindow
	   IsWindowEnabled
	   IsWindowVisible
	   IsChild
	   SetFocus
	   GetFocus
	   SetActiveWindow
	   GetActiveWindow
	   SetForegroundWindow
	   GetForegroundWindow
	   LockSetForegroundWindow
	   FindWindowExA
	   ShowWindow
	   DestroyWindow
	   GetWindowTextLengthA
	   GetWindowTextA
	   SetWindowTextA
	   DefWindowProcA

	   GetDC
	   GetWindowDC
	   ReleaseDC

	   PostQuitMessage
	   CreateAcceleratorTableA
	   TranslateAcceleratorA
	   GetMessageA
	   PostMessageA
	   PostThreadMessageA
	   WaitMessage
	   TranslateMessage
	   DispatchMessageA
	   ))

(in-package #:w32api.user32)

(define-foreign-library user32
  (:win32 "user32.dll"))

(use-foreign-library user32)

(defcfun "RegisterClassExA" C_ATOM
  (lpwcx (:pointer (:struct WNDCLASSEX))))

(defcfun "UnregisterClassA" :boolean
  (lpClassName :string)
  (hInstance HINSTANCE))

(defcfun "GetClassInfoExA" :boolean
  (hinst    HINSTANCE)
  (lpszClass      :string)
  (lpwcx (:pointer (:struct WNDCLASSEX))))

(defcfun "GetClassNameA" :boolean
  (hWnd HWND)
  (lpClassName :pointer)
  (nMaxCount :int))

(defcfun "SetClassLongPtrA" ULONG_PTR
  (hWnd     HWND)
  (nIndex      :int)
  (dwNewLong   LONG_PTR))

;; (defcfun "CreateWindowA" HWND
;;   (lpClassName   :string)
;;   (lpWindowName   :string)
;;   (dwStyle     WS_FLAG)
;;   (x :int)
;;   (y :int)
;;   (nWidth       :int)
;;   (nHeight       :int)
;;   (hWndParent      HWND)
;;   (hMenu     HMENU)
;;   (hInstance HINSTANCE)
;;   (lpParam    (:pointer :void)))

(defcfun "CreateWindowExA" HWND
  (dwExStyle     WS_EX_FLAG)
  (lpClassName   :string)
  (lpWindowName   :string)
  (dwStyle     WS_FLAG)
  (x :int)
  (y :int)
  (nWidth       :int)
  (nHeight       :int)
  (hWndParent      HWND)
  (hMenu     HMENU)
  (hInstance HINSTANCE)
  (lpParam    (:pointer :void)))

(defcfun "SetParent" HWND
  (hWndChild HWND)
  (hWndNewParent HWND))

(defcfun "SetWindowLongPtrA" LONG_PTR
  (hWnd     HWND)
  (nIndex   :int)
  (dwNewLong LONG_PTR))

(defcfun "ShowWindow" :boolean
  (hWnd HWND)
  (nCmdShow SW_ENUM))

(defcfun "AnimateWindow" :boolean
  (hwnd  HWND)
  (dwTime DWORD)
  (dwFlags DWORD))

(defcfun "EnableWindow" :boolean
  (hWnd HWND)
  (bEnable :boolean))

(defcfun "SetFocus" :boolean
  (hWnd HWND))

(defcfun "GetFocus" HWND)

(defcfun "SetActiveWindow" :boolean
  (hWnd HWND))

(defcfun "GetActiveWindow" HWND)

(defcfun "SetForegroundWindow" :boolean
  (hWnd HWND))					;

(defcfun "GetForegroundWindow" HWND)

(defcfun "LockSetForegroundWindow" :boolean
  (uLockCode :unsigned-int))

(defcfun "FindWindowExA" HWND
  (hwndParent    HWND)
  (hwndChildAfter    HWND)
  (lpszClass :string)
  (lpszWindow :string))

(defcfun "IsChild" :boolean
  (hWndParent HWND)
  (hWnd HWND))

(defcfun "IsWindow" :boolean
  (hWnd HWND))

(defcfun "IsWindowVisible" :boolean
  (hWnd HWND))

(defcfun "IsWindowEnabled" :boolean
  (hWnd HWND))

(defcfun "DestroyWindow" :boolean
  (hWnd HWND))

(defcfun "DefWindowProcA" LRESULT
  (hWnd   HWND)
  (Msg   :unsigned-int)
  (wParam WPARAM)
  (lParam LPARAM))

(defcfun "GetWindowTextLengthA" :int
  (hWnd HWND))

(defcfun "GetWindowTextA" :boolean
  (hWnd HWND)
  (lpString :string)
  (nMaxCount :int))

(defcfun "SetWindowTextA" :boolean
  (hWnd HWND)
  (lpString :string))

(defcfun "CreateAcceleratorTableA" HACCEL
  (lpaccl (:pointer (:struct ACCEL)))
  (cEntries :int))

(defcfun "TranslateAcceleratorA" :boolean
  (hWnd   HWND)
  (hAccTable HACCEL)
  (lpMsg (:pointer (:struct MSG))))

(defcfun "GetMessageA" :int 		;be aware of return -1 when attached window destroyed
  (lpMsg (:pointer (:struct MSG)))
  (hWnd  HWND)
  (wMsgFilterMin :unsigned-int)
  (wMsgFilterMax :unsigned-int))

(defcfun "PostMessageA" :boolean
  (hWnd   HWND)
  (Msg   :unsigned-int)
  (wParam WPARAM)
  (lParam LPARAM))

(defcfun "PostThreadMessageA" :boolean
  (idThread  DWORD)
  (Msg   :unsigned-int)
  (wParam WPARAM)
  (lParam LPARAM))

(defcfun "WaitMessage" :boolean)

(defcfun "TranslateMessage" :boolean
  (lpMsg (:pointer (:struct MSG))))

(defcfun "DispatchMessageA" LRESULT
  (lpMsg (:pointer (:struct MSG))))

;;; 
(defcfun "GetDC" HDC
  (hWnd HWND))

(defcfun "GetWindowDC" HDC
  (hWnd HWND))

(defcfun "ReleaseDC" :boolean
  (hWnd HWND)
  (hDC  HDC))

(defcfun "PostQuitMessage" :void
  (id :int))
