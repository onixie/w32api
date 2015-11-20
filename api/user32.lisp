(defpackage #:w32api.user32
  (:use #:common-lisp #:cffi #:w32api.type)
  (:export RegisterClassExW
	   UnregisterClassW
	   GetClassNameW
	   ;;	   CreateWindowA
	   CreateWindowExW
	   SetParent
	   GetParent
	   GetAncestor
	   SetWindowLongPtrW
	   GetWindowLongPtrW
	   SetWindowStyle
	   GetWindowStyle
	   AnimateWindow
	   EnableWindow
	   IsWindow
	   IsWindowEnabled
	   IsWindowVisible
	   IsChild
	   IsIconic
	   IsZoomed
	   SwitchToThisWindow
	   SetFocus
	   GetFocus
	   SetActiveWindow
	   GetActiveWindow
	   SetForegroundWindow
	   GetForegroundWindow
	   LockSetForegroundWindow
	   CloseWindow
	   OpenIcon
	   FindWindowExW
	   ShowWindow
	   DestroyWindow
	   GetWindowTextLengthW
	   GetWindowTextW
	   SetWindowTextW
	   DefWindowProcW

	   UpdateWindow
	   GetDC
	   GetWindowDC
	   ReleaseDC
	   WindowFromDC
	   BeginPaint
	   EndPaint
	   
	   PostQuitMessage
	   CreateAcceleratorTableW
	   TranslateAcceleratorW
	   GetMessageW
	   PostMessageW
	   PostThreadMessageW
	   WaitMessage
	   TranslateMessage
	   DispatchMessageW
	   ))

(in-package #:w32api.user32)

(define-foreign-library user32
  (:win32 "user32.dll"))

(use-foreign-library user32)

(defcfun "RegisterClassExW" C_ATOM
  (lpwcx (:pointer (:struct WNDCLASSEX))))

(defcfun "UnregisterClassW" :boolean
  (lpClassName :string)
  (hInstance HINSTANCE))

(defcfun "GetClassInfoExW" :boolean
  (hinst    HINSTANCE)
  (lpszClass      :string)
  (lpwcx (:pointer (:struct WNDCLASSEX))))

(defcfun "GetClassNameW" :boolean
  (hWnd HWND)
  (lpClassName :pointer)
  (nMaxCount :int))

(defcfun "SetClassLongPtrW" ULONG_PTR
  (hWnd     HWND)
  (nIndex      :int)
  (dwNewLong   LONG_PTR))

;; (defcfun "CreateWindowW" HWND
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

(defcfun "CreateWindowExW" HWND
  (dwExStyle     WS_EX_FLAG)
  (lpClassName   :string)
  (lpWindowName   :string)
  (dwStyle (bitfield-union DWORD WS_FLAG BS_FLAG))
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

(defcfun "GetParent" HWND
  (hWnd HWND))

(defcfun "GetAncestor" HWND
  (hWnd HWND)
  (gaFlags GA_ENUM))

(defcfun "SetWindowLongPtrW" LONG_PTR
  (hWnd     HWND)
  (nIndex   :int)
  (dwNewLong LONG_PTR))

(defcfun "GetWindowLongPtrW" LONG_PTR
  (hWnd     HWND)
  (nIndex   :int))

(defun SetWindowStyle (hWnd styles)
  (foreign-funcall "SetWindowLongPtrW"
		   HWND hWnd
		   GWLP_ENUM :STYLE
		   (bitfield-union DWORD WS_FLAG BS_FLAG) styles
		   (bitfield-union DWORD WS_FLAG BS_FLAG)))

(defun GetWindowStyle (hWnd)
  (foreign-funcall "GetWindowLongPtrW"
		   HWND hWnd
		   GWLP_ENUM :STYLE
		   (bitfield-union DWORD WS_FLAG BS_FLAG)))

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

(defcfun "SwitchToThisWindow" :void
  (hWnd HWND)
  (fAltTab :boolean))

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

(defcfun "CloseWindow" :boolean
  (hWnd HWND))

(defcfun "OpenIcon" :boolean
  (hWnd HWND))

(defcfun "FindWindowExW" HWND
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

(defcfun "IsIconic" :boolean
  (hWnd HWND))

(defcfun "IsZoomed" :boolean
  (hWnd HWND))

(defcfun "DestroyWindow" :boolean
  (hWnd HWND))

(defcfun "DefWindowProcW" LRESULT
  (hWnd   HWND)
  (Msg   :unsigned-int)
  (wParam WPARAM)
  (lParam LPARAM))

(defcfun "GetWindowTextLengthW" :int
  (hWnd HWND))

(defcfun "GetWindowTextW" :boolean
  (hWnd HWND)
  (lpString :string)
  (nMaxCount :int))

(defcfun "SetWindowTextW" :boolean
  (hWnd HWND)
  (lpString :string))


(defcfun "CreateAcceleratorTableW" HACCEL
  (lpaccl (:pointer (:struct ACCEL)))
  (cEntries :int))

(defcfun "TranslateAcceleratorW" :boolean
  (hWnd   HWND)
  (hAccTable HACCEL)
  (lpMsg (:pointer (:struct MSG))))

(defcfun "GetMessageW" :int 		;be aware of return -1 when attached window destroyed
  (lpMsg (:pointer (:struct MSG)))
  (hWnd  HWND)
  (wMsgFilterMin :unsigned-int)
  (wMsgFilterMax :unsigned-int))

(defcfun "PostMessageW" :boolean
  (hWnd   HWND)
  (Msg   :unsigned-int)
  (wParam WPARAM)
  (lParam LPARAM))

(defcfun "PostThreadMessageW" :boolean
  (idThread  DWORD)
  (Msg   :unsigned-int)
  (wParam WPARAM)
  (lParam LPARAM))

(defcfun "WaitMessage" :boolean)

(defcfun "TranslateMessage" :boolean
  (lpMsg (:pointer (:struct MSG))))

(defcfun "DispatchMessageW" LRESULT
  (lpMsg (:pointer (:struct MSG))))

(defcfun "PostQuitMessage" :void
  (id :int))

;;; 
(defcfun "UpdateWindow" :boolean
  (hWnd HWND))

(defcfun "GetDC" HDC
  (hWnd HWND))

(defcfun "GetWindowDC" HDC
  (hWnd HWND))

(defcfun "ReleaseDC" :boolean
  (hWnd HWND)
  (hDC  HDC))

(defcfun "WindowFromDC" HWND
  (hDC  HDC))

(defcfun "BeginPaint" HDC
  (hwnd          HWND)
  (lpPaint (:pointer (:struct PAINTSTRUCT))))

(defcfun "EndPaint" :boolean
  (hwnd          HWND)
  (lpPaint (:pointer (:struct PAINTSTRUCT))))
