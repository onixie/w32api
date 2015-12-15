(defpackage #:w32api.user32
  (:use #:common-lisp #:cffi #:w32api.type)
  (:export GetSystemMetrics
	   GetProcessWindowStation
	   EnumDesktopsW
	   CreateDesktopW
	   OpenDesktopW
	   OpenInputDesktop
	   SwitchDesktop
	   CloseDesktop
	   SetThreadDesktop
	   GetThreadDesktop
	   RegisterClassExW
	   UnregisterClassW
	   DefWindowProcW
	   CallWindowProcW
	   GetClassNameW
	   GetClassInfoExW
	   GetClassLongPtrW
	   SetClassLongPtrW
	   ;;	   CreateWindowA
	   CreateWindowExW
	   FindWindowExW
	   GetDesktopWindow
	   SetParent
	   GetParent
	   GetAncestor
	   GetWindow
	   EnumChildWindows
	   EnumWindows
	   EnumDesktopWindows
	   EnumThreadWindows
	   GetTopWindow
	   GetWindowThreadProcessId
	   GetWindowThreadId
	   GetWindowProcessId
	   GetWindowTextLengthW
	   GetWindowTextW
	   SetWindowTextW
	   SetWindowLongPtrW
	   GetWindowLongPtrW
	   SetWindowStyle
	   GetWindowStyle
	   SetFocus
	   GetFocus
	   SetActiveWindow
	   GetActiveWindow
	   SetForegroundWindow
	   GetForegroundWindow
	   LockSetForegroundWindow
	   CloseWindow
	   OpenIcon
	   ShowWindow
	   AnimateWindow
	   EnableWindow
	   SwitchToThisWindow
	   BringWindowToTop
	   UpdateWindow
	   MoveWindow
	   DestroyWindow
	   CascadeWindows
	   TileWindows

	   IsWindow
	   IsWindowUnicode
	   IsWindowEnabled
	   IsWindowVisible
	   IsChild
	   IsIconic
	   IsZoomed

	   GetDC
	   GetWindowDC
	   ReleaseDC
	   WindowFromDC
	   BeginPaint
	   EndPaint
	   GetWindowRect
	   GetClientRect
	   InvalidateRect
	   ValidateRect
	   InvalidateRgn
	   ValidateRgn
	   GetWindowRgn
	   SetWindowRgn

	   CreateAcceleratorTableW
	   TranslateAcceleratorW
	   GetMessageW
	   PeekMessageW
	   PostMessageW
	   SendMessageW
	   PostThreadMessageW
	   PostQuitMessage
	   WaitMessage
	   TranslateMessage
	   DispatchMessageW

	   GetSysColor	   
	   GetSysColorBrush
	   MessageBoxW
	   ))

(in-package #:w32api.user32)

(define-foreign-library user32
  (:win32 "user32.dll"))

(use-foreign-library "user32")

(defcfun "GetProcessWindowStation" HWINSTA)

(defcfun "EnumDesktopsW" :boolean
  (hwinsta         HWINSTA)
  (lpEnumFunc :pointer)
  (lParam          LPARAM)
  )

(defcfun "CreateDesktopW"  HDESK
  (lpszDesktop               :string)
  (lpszDevice               :string)
  (pDevmode               (:pointer (:struct DEVMODE)))
  (dwFlags                 DF_FLAG)
  (dwDesiredAccess         DA_FLAG)
  (lpsa (:pointer (:struct SECURITY_ATTRIBUTES)))
  )

(defcfun "OpenDesktopW" HDESK
  (lpszDesktop      :string)
  (dwFlags       DF_FLAG)
  (fInherit        :boolean)
  (dwDesiredAccess DA_FLAG))

(defcfun "OpenInputDesktop" HDESK
  (dwFlags       DF_FLAG)
  (fInherit        :boolean)
  (dwDesiredAccess DA_FLAG))

(defcfun "SwitchDesktop" :boolean
  (hDesktop HDESK))

(defcfun "CloseDesktop" :boolean
  (hDesktop HDESK))

(defcfun "SetThreadDesktop" :boolean
  (hDesktop HDESK))

(defcfun "GetThreadDesktop" HDESK
  (dwThreadId DWORD))

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
  (hWnd        HWND)
  (nIndex      GCL_ENUM)
  (dwNewLong   LONG_PTR))

(defcfun "GetClassLongPtrW" ULONG_PTR
  (hWnd        HWND)
  (nIndex      GCL_ENUM))

(defcfun "CreateWindowExW" HWND
  (dwExStyle     WS_EX_FLAG)
  (lpClassName   :string)
  (lpWindowName   :string)
  (dwStyle WND_STYLE)
  (x :int)
  (y :int)
  (nWidth       :int)
  (nHeight       :int)
  (hWndParent      HWND)
  (hMenu     HMENU)
  (hInstance HINSTANCE)
  (lpParam    (:pointer :void)))

(defcfun "GetWindow" HWND
  (hWnd HWND)
  (uCmd GW_ENUM))

(defcfun "GetTopWindow" HWND ; = (GetWindow ... :CHILD)
  (hWnd HWND))

(defcfun "GetDesktopWindow" HWND)

(defcfun "SetParent" HWND
  (hWndChild HWND)
  (hWndNewParent HWND))

(defcfun "GetParent" HWND
  (hWnd HWND))

(defcfun "GetAncestor" HWND
  (hWnd HWND)
  (gaFlags GA_ENUM))

(defcfun "EnumDesktopWindows" :boolean
  (hDesktop       HDESK)
  (lpfn :pointer)
  (lParam      LPARAM)
  )

(defcfun "EnumWindows" :boolean
  (lpEnumFunc :pointer)
  (lParam      LPARAM)
  )

(defcfun "EnumChildWindows" :boolean
  (hWndParent HWND)
  (lpEnumFunc :pointer)
  (lParam      LPARAM))

(defcfun "EnumThreadWindows" :boolean
  (dwThreadId DWORD)
  (lpfn       :pointer)
  (lParam     LPARAM))

(defcfun "SetWindowLongPtrW" LONG_PTR
  (hWnd     HWND)
  (nIndex   GWLP_ENUM)
  (dwNewLong LONG_PTR))

(defcfun "GetWindowLongPtrW" LONG_PTR
  (hWnd     HWND)
  (nIndex   GWLP_ENUM))

(defun SetWindowStyle (hWnd styles)
  (foreign-funcall "SetWindowLongPtrW"
		   HWND hWnd
		   GWLP_ENUM :GWL_STYLE
		   WND_STYLE styles
		   WND_STYLE))

(defun GetWindowStyle (hWnd)
  (foreign-funcall "GetWindowLongPtrW"
		   HWND hWnd
		   GWLP_ENUM :GWL_STYLE
		   WND_STYLE))

(defcfun "GetWindowThreadProcessId" DWORD
  (hWnd    HWND)
  (lpdwProcessId (:pointer DWORD)))

(defun GetWindowThreadId (hWnd)
  (foreign-funcall "GetWindowThreadProcessId"
		   HWND hWnd
		   (:pointer DWORD) (null-pointer)
		   DWORD))

(defun GetWindowProcessId (hWnd)
  (with-foreign-object (procId 'DWORD)
    (foreign-funcall "GetWindowThreadProcessId"
		     HWND hWnd
		     (:pointer DWORD) procId
		     DWORD)
    (mem-ref procId 'DWORD)))

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

(defcfun "BringWindowToTop" :void
  (hWnd HWND))

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
  (uLockCode :uint))

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

(defcfun "IsWindowUnicode" :boolean
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
  (Msg    WND_MESSAGE)
  (wParam WPARAM)
  (lParam LPARAM))

(defcfun "CallWindowProcW" LRESULT
  (lpPrevWndFunc :pointer)
  (hWnd   HWND)
  (Msg    WND_MESSAGE)
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

(defcfun "TileWindows" WORD
  (hwndParent HWND)
  (wHow MDITILE_FLAG)
  (lpRect (:pointer (:struct  RECT)))
  (cKids :uint)
  (lpKids (:pointer HWND)))

(defcfun "CascadeWindows" WORD
  (hwndParent HWND)
  (wHow MDITILE_FLAG)
  (lpRect (:pointer (:struct  RECT)))
  (cKids :uint)
  (lpKids (:pointer HWND)))

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
  (wMsgFilterMin :uint)
  (wMsgFilterMax :uint))

(defcfun "PeekMessageW" :boolean
  (lpMsg (:pointer (:struct MSG)))
  (hWnd  HWND)
  (wMsgFilterMin :uint)
  (wMsgFilterMax :uint)
  (wRemoveMsg PM_FLAG)
  )

(defcfun "PostMessageW" :boolean
  (hWnd   HWND)
  (Msg    WND_MESSAGE)
  (wParam WPARAM)
  (lParam LPARAM))

(defcfun "SendMessageW" :boolean
  (hWnd   HWND)
  (Msg    WND_MESSAGE)
  (wParam WPARAM)
  (lParam LPARAM))

(defcfun "PostThreadMessageW" :boolean
  (idThread DWORD)
  (Msg      WND_MESSAGE)
  (wParam   WPARAM)
  (lParam   LPARAM))

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

(defcfun "GetWindowRect" :boolean
  (hwnd          HWND)
  (lpRect (:pointer (:struct RECT))))

(defcfun "GetClientRect" :boolean
  (hwnd          HWND)
  (lpRect (:pointer (:struct RECT))))

(defcfun "MoveWindow" :boolean
  (hwnd          HWND)
  (X :int)
  (Y :int)
  (nWidth :int)
  (nHeight :int)
  (bRepaint :boolean))

(defcfun "InvalidateRect" :boolean
  (hWnd HWND)
  (lpRect (:pointer (:struct RECT)))
  (bErase :boolean))

(defcfun "ValidateRect" :boolean
  (hWnd HWND)
  (lpRect (:pointer (:struct RECT))))

(defcfun "InvalidateRgn" :boolean
  (hWnd HWND)
  (hRgn HRGN)
  (bErase :boolean))

(defcfun "ValidateRgn" :boolean
  (hWnd HWND)
  (hRgn HRGN))

(defcfun "GetWindowRgn" GWR_RESULT_ENUM
  (hWnd HWND)
  (hRgn HRGN))

(defcfun "SetWindowRgn" :boolean
  (hWnd HWND)
  (hRgn HRGN)
  (bRedraw :boolean))

(defcfun "WindowFromPoint" HWND
  (Point (:pointer (:struct POINT))))

(defcfun "WindowFromPhysicalPoint" HWND
  (Point (:pointer (:struct POINT))))

(defcfun "ChildWindowFromPointEx" HWND
  (hWndParent  HWND)
  (Point (:pointer (:struct POINT)))
  (uFlags  :uint))

(defcfun "RealChildWindowFromPoint" HWND
  (hWndParent  HWND)
  (ptParentClientCoords (:pointer (:struct POINT))))

(defcfun "GetSystemMetrics" :int
  (nIndex SM_ENUM))

(defcfun "GetSysColorBrush" HBRUSH
  (nIndex COLOR_ENUM))

(defcfun "GetSysColor" DWORD
  (nIndex COLOR_ENUM))

(defcfun "MessageBoxW" MB_RESULT_ENUM
  (hWnd      HWND)
  (lpText    :string)
  (lpCaption :string)
  (uType     MB_FLAG))
