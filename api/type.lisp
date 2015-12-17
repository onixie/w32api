(defpackage #:w32api.type
  (:use #:common-lisp #:cffi #:w32api.util)
  (:export bitfield-union
	   +CW_USEDEFAULT+
	   +DWLP_DLGPROC+
	   +DWLP_MSGRESULT+
	   +DWLP_USER+
	   +HWND_BROADCAST+
	   +HWND_MESSAGE+
	   +WS_EX_OVERLAPPEDWINDOW+
	   +WS_EX_PALETTEWINDOW+
	   +WS_OVERLAPPEDWINDOW+
	   +WS_POPUPWINDOW+
	   +WS_TILEDWINDOW+
	   +window-class-name-max-length+
	   ACCEL
	   ACCEL_VIRT_FLAG
	   AW_FLAG
	   BS_FLAG
	   COMPUTER_NAME_FORMAT_ENUM
	   CS_FLAG
	   COLOR_ENUM
	   C_ATOM
	   C_BYTE
	   DA_FLAG
	   DEVMODE
	   DF_FLAG
	   DLGPROC
	   DWORD
	   DWORD_PTR
	   ES_FLAG
	   EXTENDED_NAME_FORMAT_ENUM
	   FIRMWARE_TYPE_ENUM
	   FORMAT_MESSAGE_FLAG
	   GA_ENUM
	   GCL_ENUM
	   GWLP_ENUM
	   GW_ENUM
	   GWR_RESULT_ENUM
	   HANDLE
	   HACCEL
	   HBRUSH
	   HCURSOR
	   HDC
	   HDESK
	   HMONITOR
	   HIBYTE
	   HICON
	   HINSTANCE
	   HIWORD
	   HRGN
	   HMENU
	   HMODULE
	   HWINSTA
	   HWND
	   INT_PTR
	   LOBYTE
	   LONG_PTR
	   SSIZE_T
	   SIZE_T
	   LOWORD
	   LPARAM
	   LPVOID
	   LRESULT
	   MSG
	   MAKELANGID
	   MDITILE_FLAG
	   OSVERSIONINFOEX
	   PAINTSTRUCT
	   POINT
	   PRODUCT_ENUM
	   PM_FLAG
	   RECT
	   SECURITY_ATTRIBUTES
	   SM_ENUM
	   SW_ENUM
	   SYSTEM_INFO
	   SYSTEM_INFO_ARCH
	   SYSTEM_INFO_MISC
	   UINT_PTR
	   ULONG_PTR
	   VER_NT_FLAG
	   VER_SUITE_FLAG
	   WM_ENUM
	   BM_ENUM
	   BN_ENUM
	   WNDCLASSEX
	   WND_MESSAGE
	   WND_STYLE
	   WORD
	   WPARAM
	   WS_EX_FLAG
	   WS_FLAG
	   enum-union
	   PF_ENUM
	   MB_FLAG
	   MB_RESULT_ENUM
	   MONITOR_FLAG
	   MONITORINFOEX
	   TH32CS_FLAG
	   +TH32CS_SNAPALL+
	   HEAPLIST32
	   HEAPENTRY32
	   MODULEENTRY32
	   PROCESSENTRY32
	   THREADENTRY32
	   THREAD_FLAG
	   +THREAD_ALL_ACCESS+
	   PROCESS_FLAG
	   +PROCESS_ALL_ACCESS+
	   WAIT_RESULT_ENUM
	   UOI_ENUM
	   USEROBJECTFLAGS
	   ))

(in-package #:w32api.type)

(defparameter +window-class-name-max-length+ 256)
(setf *default-foreign-encoding* :utf-16le)

(defmacro LOWORD (data)
  `(ldb (byte 16 0)
	(car (list ,data))))

(defmacro HIWORD (data)
  `(ldb (byte 16 16)
	(car (list ,data))))

(defmacro LOBYTE (data)
  `(ldb (byte 8 0)
	(car (list ,data))))

(defmacro HIBYTE (data)
  `(ldb (byte 8 8)
	(car (list ,data))))

#+x86-64
(progn
  (defctype INT_PTR	:int64)
  (defctype UINT_PTR	:uint64)
  (defctype LONG_PTR	:int64)
  (defctype ULONG_PTR	:uint64))
#+x86
(progn
  (defctype INT_PTR	:int32)
  (defctype UINT_PTR	:uint32)
  (defctype LONG_PTR	:int32)
  (defctype ULONG_PTR	:uint32))

(defctype C_BYTE	:uchar)
(defctype WORD		:ushort)
(defctype DWORD		:ulong)
(defctype LPVOID        (:pointer :void))
(defctype DWORD_PTR     ULONG_PTR)
(defctype SIZE_T        ULONG_PTR)
(defctype SSIZE_T       LONG_PTR)

(defctype HANDLE	:pointer)
(defctype HWINSTA	:pointer)
(defctype HDESK		:pointer)
(defctype HMONITOR	:pointer)
(defctype HWND		:pointer)
(defctype HDC		:pointer)
(defctype HINSTANCE	:pointer)
(defctype HMODULE	:pointer)
(defctype HICON		:pointer)
(defctype HCURSOR	:pointer)
(defctype HBRUSH	:pointer)
(defctype HMENU		:pointer)
(defctype HACCEL	:pointer)
(defctype HRGN          :pointer)

(defctype LRESULT LONG_PTR)
(defctype LPARAM  LONG_PTR)
(defctype WPARAM  UINT_PTR)
(defctype C_ATOM  WORD)

(defctype ACCESS_MASK DWORD)

(defbitfield (STANDARD_RIGHTS_FLAG ACCESS_MASK)
  (:DELETE			#x00010000)	;Required to delete the object.
  (:READ_CONTROL		#x00020000)	;Required to read information in the security descriptor for the object, not including the information in the SACL. To read or write the SACL, you must request the ACCESS_SYSTEM_SECURITY access right. For more information, see SACL Access Right.
  (:SYNCHRONIZE			#x00100000)	;Not supported for desktop objects.
  (:WRITE_DAC			#x00040000)	;Required to modify the DACL in the security descriptor for the object.
  (:WRITE_OWNER			#x00080000)	;Required to change the owner in the security descriptor for the object.
  )

(defparameter +STANDARD_RIGHTS_ALL+
  '(:DELETE
    :READ_CONTROL
    :WRITE_DAC
    :WRITE_OWNER
    :SYNCHRONIZE))

(defparameter +STANDARD_RIGHTS_REQUIRED+
  '(:DELETE
    :READ_CONTROL
    :WRITE_DAC
    :WRITE_OWNER))

(defparameter +STANDARD_RIGHTS_EXECUTE+
  '(:READ_CONTROL))

(defparameter +STANDARD_RIGHTS_READ+
  '(:READ_CONTROL))

(defparameter +STANDARD_RIGHTS_WRITE+
  '(:READ_CONTROL))

;;; CreateDesktop dwFlags

(defbitfield (DF_FLAG DWORD)
  (:DF_ALLOWOTHERACCOUNTHOOK #x0001)		;Enables processes running in other accounts on the desktop to set hooks in this process.
  (:WSF_VISIBLE              #x0001))

(defbitfield (%DA_FLAG ACCESS_MASK)
  (:DESKTOP_CREATEMENU		#x0004)		;Required to create a menu on the desktop.
  (:DESKTOP_CREATEWINDOW	#x0002)		;Required to create a window on the desktop.
  (:DESKTOP_ENUMERATE		#x0040)		;Required for the desktop to be enumerated.
  (:DESKTOP_HOOKCONTROL		#x0008)		;Required to establish any of the window hooks.
  (:DESKTOP_JOURNALPLAYBACK	#x0020)		;Required to perform journal playback on a desktop.
  (:DESKTOP_JOURNALRECORD	#x0010)		;Required to perform journal recording on a desktop.
  (:DESKTOP_READOBJECTS		#x0001)		;Required to read objects on the desktop.
  (:DESKTOP_SWITCHDESKTOP	#x0100)		;Required to activate the desktop using the SwitchDesktop function.
  (:DESKTOP_WRITEOBJECTS	#x0080)		;Required to write objects on the desktop.
  )

(defctype DA_FLAG (bitfield-union ACCESS_MASK STANDARD_RIGHTS_FLAG %DA_FLAG))

(defparameter +DESKTOP_GENERIC_READ+
  (list*
   :DESKTOP_ENUMERATE
   :DESKTOP_READOBJECTS
   +STANDARD_RIGHTS_READ+))

(defparameter +DESKTOP_GENERIC_WRITE+
  (list*
   :DESKTOP_CREATEMENU
   :DESKTOP_CREATEWINDOW
   :DESKTOP_HOOKCONTROL
   :DESKTOP_JOURNALPLAYBACK
   :DESKTOP_JOURNALRECORD
   :DESKTOP_WRITEOBJECTS
   +STANDARD_RIGHTS_WRITE+))

(defparameter +DESKTOP_GENERIC_EXECUTE+
  (list* :DESKTOP_SWITCHDESKTOP +STANDARD_RIGHTS_EXECUTE+))

(defparameter +DESKTOP_GENERIC_ALL+
  (list* :DESKTOP_CREATEMENU
	 :DESKTOP_CREATEWINDOW
	 :DESKTOP_ENUMERATE
	 :DESKTOP_HOOKCONTROL
	 :DESKTOP_JOURNALPLAYBACK
	 :DESKTOP_JOURNALRECORD
	 :DESKTOP_READOBJECTS
	 :DESKTOP_SWITCHDESKTOP
	 :DESKTOP_WRITEOBJECTS
	 +STANDARD_RIGHTS_REQUIRED+
	 ))

(defcstruct SECURITY_ATTRIBUTES
  (:nLength              DWORD)
  (:lpSecurityDescriptor LPVOID)
  (:bInheritHandle       :boolean)
  )

;;;
(defctype DLGPROC :pointer)

(defparameter +CW_USEDEFAULT+ (- 0 #x80000000))

(defparameter +HWND_MESSAGE+ -3)

(defbitfield (CS_FLAG :uint)
  (:CS_BYTEALIGNCLIENT	#x1000)		; Aligns the window's client area on a byte boundary (in the x direction). This style affects the width of the window and its horizontal placement on the display.
  (:CS_BYTEALIGNWINDOW	#x2000)		; Aligns the window on a byte boundary (in the x direction). This style affects the width of the window and its horizontal placement on the display.
  (:CS_CLASSDC		#x0040)		; Allocates one device context to be shared by all windows in the class. Because window classes are process specific, it is possible for multiple threads of an application to create a window of the same class. It is also possible for the threads to attempt to use the device context simultaneously. When this happens, the system allows only one thread to successfully finish its drawing operation.
  (:CS_DBLCLKS		#x0008)		; Sends a double-click message to the window procedure when the user double-clicks the mouse while the cursor is within a window belonging to the class.
  (:CS_DROPSHADOW	#x00020000)	; Enables the drop shadow effect on a window. The effect is turned on and off through SPI_SETDROPSHADOW. Typically, this is enabled for small, short-lived windows such as menus to emphasize their Z-order relationship to other windows. Windows created from a class with this style must be top-level windows; they may not be child windows.
  (:CS_GLOBALCLASS	#x4000)		; Indicates that the window class is an application global class. For more information, see the "Application Global Classes" section of About Window Classes.
  (:CS_HREDRAW		#x0002)		; Redraws the entire window if a movement or size adjustment changes the width of the client area.
  (:CS_NOCLOSE		#x0200)		; Disables Close on the window menu.
  (:CS_OWNDC		#x0020)		; Allocates a unique device context for each window in the class.
  (:CS_PARENTDC		#x0080)		; Sets the clipping rectangle of the child window to that of the parent window so that the child can draw on the parent. A window with the CS_PARENTDC style bit receives a regular device context from the system's cache of device contexts. It does not give the child the parent's device context or device context settings. Specifying CS_PARENTDC enhances an application's performance.
  (:CS_SAVEBITS		#x0800)		; Saves, as a bitmap, the portion of the screen image obscured by a window of this class. When the window is removed, the system uses the saved bitmap to restore the screen image, including other windows that were obscured. Therefore, the system does not send WM_PAINT messages to windows that were obscured if the memory used by the bitmap has not been discarded and if other screen actions have not invalidated the stored image.This style is useful for small windows (for example, menus or dialog boxes) that are displayed briefly and then removed before other screen activity takes place. This style increases the time required to display the window, because the system must first allocate memory to store the bitmap.
  (:CS_VREDRAW		#x0001))	; Redraws the entire window if a movement or size adjustment changes the height of the client area.

(defbitfield (BS_FLAG DWORD)
  (:BS_PUSHBUTTON       #x00000000)
  (:BS_DEFPUSHBUTTON    #x00000001)
  (:BS_CHECKBOX         #x00000002)
  (:BS_AUTOCHECKBOX     #x00000003)
  (:BS_RADIOBUTTON      #x00000004)
  (:BS_3STATE           #x00000005)
  (:BS_AUTO3STATE       #x00000006)
  (:BS_GROUPBOX         #x00000007)
  (:BS_USERBUTTON       #x00000008)
  (:BS_AUTORADIOBUTTON  #x00000009)
  (:BS_PUSHBOX          #x0000000A)
  (:BS_OWNERDRAW        #x0000000B)
  (:BS_TYPEMASK         #x0000000F)
  (:BS_LEFTTEXT         #x00000020)

  (:BS_TEXT             #x00000000);below is for winver > 4
  (:BS_ICON             #x00000040)
  (:BS_BITMAP           #x00000080)
  (:BS_LEFT             #x00000100)
  (:BS_RIGHT            #x00000200)
  (:BS_CENTER           #x00000300)
  (:BS_TOP              #x00000400)
  (:BS_BOTTOM           #x00000800)
  (:BS_VCENTER          #x00000C00)
  (:BS_PUSHLIKE         #x00001000)
  (:BS_MULTILINE        #x00002000)
  (:BS_NOTIFY           #x00004000)
  (:BS_FLAT             #x00008000)
  )

(defbitfield (ES_FLAG DWORD)
  (:ES_LEFT             #x0000)
  (:ES_CENTER           #x0001)
  (:ES_RIGHT            #x0002)
  (:ES_MULTILINE        #x0004)
  (:ES_UPPERCASE        #x0008)
  (:ES_LOWERCASE        #x0010)
  (:ES_PASSWORD         #x0020)
  (:ES_AUTOVSCROLL      #x0040)
  (:ES_AUTOHSCROLL      #x0080)
  (:ES_NOHIDESEL        #x0100)
  (:ES_OEMCONVERT       #x0400)
  (:ES_READONLY         #x0800)
  (:ES_WANTRETURN       #x1000)
					;>=0x0400
  (:ES_NUMBER           #x2000))

(defctype WND_STYLE (bitfield-union DWORD WS_FLAG BS_FLAG ES_FLAG))

;;; Get/SetClassLongPtr nIndex
(defcenum (GCL_ENUM :int)
  (:GCW_ATOM            -32)    ;Retrieves an ATOM value that uniquely identifies the window class. This is the same atom that the RegisterClassEx function returns.
  (:GCL_CBCLSEXTRA	-20)	;Sets the size, in bytes, of the extra memory associated with the class. Setting this value does not change the number of extra bytes already allocated.
  (:GCL_CBWNDEXTRA	-18)	;Sets the size, in bytes, of the extra window memory associated with each window in the class. Setting this value does not change the number of extra bytes already allocated. For information on how to access this memory, see SetWindowLongPtr.
  (:GCLP_HBRBACKGROUND	-10)	;Replaces a handle to the background brush associated with the class.
  (:GCLP_HCURSOR	-12)	;Replaces a handle to the cursor associated with the class.
  (:GCLP_HICON		-14)	;Replaces a handle to the icon associated with the class.
  (:GCLP_HICONSM	-34)	;Retrieves a handle to the small icon associated with the class.
  (:GCLP_HMODULE	-16)	;Replaces a handle to the module that registered the class.
  (:GCLP_MENUNAME	-8)	;Replaces the pointer to the menu name string. The string identifies the menu resource associated with the class.
  (:GCL_STYLE		-26)	;Replaces the window-class style bits.
  (:GCLP_WNDPROC	-24))	;Replaces the pointer to the window procedure associated with the class.

(defcstruct WNDCLASSEX
  (:cbSize        :uint)
  (:style         CS_FLAG)
  (:lpfnWndProc   :pointer)
  (:cbClsExtra    :int)
  (:cbWndExtra    :int)
  (:hInstance     HINSTANCE)
  (:hIcon         HICON)
  (:hCursor       HCURSOR)
  (:hbrBackground HBRUSH)
  (:lpszMenuName  :string)
  (:lpszClassName :string)
  (:hIconSm       HICON)
  )

;; Extended Window Styles
(defbitfield (WS_EX_FLAG DWORD)
  (:WS_EX_ACCEPTFILES		#X00000010)	;The window accepts drag-drop files.
  (:WS_EX_APPWINDOW		#X00040000)	;Forces a top-level window onto the taskbar when the window is visible.
  (:WS_EX_CLIENTEDGE		#X00000200)	;The window has a border with a sunken edge.
  (:WS_EX_COMPOSITED		#X02000000)	;Paints all descendants of a window in bottom-to-top painting order using double-buffering. For more information, see Remarks. This cannot be used if the window has a class style of either CS_OWNDC or CS_CLASSDC. Windows 2000:  This style is not supported.
  (:WS_EX_CONTEXTHELP		#X00000400)	;The title bar of the window includes a question mark. When the user clicks the question mark, the cursor changes to a question mark with a pointer. If the user then clicks a child window, the child receives a WM_HELP message. The child window should pass the message to the parent window procedure, which should call the WinHelp function using the HELP_WM_HELP command. The Help application displays a pop-up window that typically contains help for the child window.WS_EX_CONTEXTHELP cannot be used with the WS_MAXIMIZEBOX or WS_MINIMIZEBOX styles.
  (:WS_EX_CONTROLPARENT		#X00010000)	;The window itself contains child windows that should take part in dialog box navigation. If this style is specified, the dialog manager recurses into children of this window when performing navigation operations such as handling the TAB key, an arrow key, or a keyboard mnemonic.
  (:WS_EX_DLGMODALFRAME		#X00000001)	;The window has a double border; the window can, optionally, be created with a title bar by specifying the WS_CAPTION style in the dwStyle parameter.
  (:WS_EX_LAYERED		#X0008000)	;The window is a layered window. This style cannot be used if the window has a class style of either CS_OWNDC or CS_CLASSDC.Windows 8:  The WS_EX_LAYERED style is supported for top-level windows and child windows. Previous Windows versions support WS_EX_LAYERED only for top-level windows.
  (:WS_EX_LAYOUTRTL		#X00400000)	;If the shell language is Hebrew, Arabic, or another language that supports reading order alignment, the horizontal origin of the window is on the right edge. Increasing horizontal values advance to the left.
  (:WS_EX_LEFT			#X00000000)	;The window has generic left-aligned properties. This is the default.
  (:WS_EX_LEFTSCROLLBAR		#X00004000)	;If the shell language is Hebrew, Arabic, or another language that supports reading order alignment, the vertical scroll bar (if present) is to the left of the client area. For other languages, the style is ignored.
  (:WS_EX_LTRREADING		#X00000000)	;The window text is displayed using left-to-right reading-order properties. This is the default.
  (:WS_EX_MDICHILD		#X00000040)	;The window is a MDI child window.
  (:WS_EX_NOACTIVATE		#X08000000)	;A top-level window created with this style does not become the foreground window when the user clicks it. The system does not bring this window to the foreground when the user minimizes or closes the foreground window.To activate the window, use the SetActiveWindow or SetForegroundWindow function.The window does not appear on the taskbar by default. To force the window to appear on the taskbar, use the WS_EX_APPWINDOW style.
  (:WS_EX_NOINHERITLAYOUT	#X00100000)	;The window does not pass its window layout to its child windows.
  (:WS_EX_NOPARENTNOTIFY	#X00000004)	;The child window created with this style does not send the WM_PARENTNOTIFY message to its parent window when it is created or destroyed.
  (:WS_EX_NOREDIRECTIONBITMAP	#X00200000)	;The window does not render to a redirection surface. This is for windows that do not have visible content or that use mechanisms other than surfaces to provide their visual.
  (:WS_EX_RIGHT			#X00001000)	;The window has generic "right-aligned" properties. This depends on the window class. This style has an effect only if the shell language is Hebrew, Arabic, or another language that supports reading-order alignment; otherwise, the style is ignored. Using the WS_EX_RIGHT style for static or edit controls has the same effect as using the SS_RIGHT or ES_RIGHT style, respectively. Using this style with button controls has the same effect as using BS_RIGHT and BS_RIGHTBUTTON styles.
  (:WS_EX_RIGHTSCROLLBAR	#X00000000)	;The vertical scroll bar (if present) is to the right of the client area. This is the default.
  (:WS_EX_RTLREADING		#X00002000)	;If the shell language is Hebrew, Arabic, or another language that supports reading-order alignment, the window text is displayed using right-to-left reading-order properties. For other languages, the style is ignored.
  (:WS_EX_STATICEDGE		#X00020000)	;The window has a three-dimensional border style intended to be used for items that do not accept user input.
  (:WS_EX_TOOLWINDOW		#X00000080)	;The window is intended to be used as a floating toolbar. A tool window has a title bar that is shorter than a normal title bar, and the window title is drawn using a smaller font. A tool window does not appear in the taskbar or in the dialog that appears when the user presses ALT+TAB. If a tool window has a system menu, its icon is not displayed on the title bar. However, you can display the system menu by right-clicking or by typing ALT+SPACE.
  (:WS_EX_TOPMOST		#X00000008)	;The window should be placed above all non-topmost windows and should stay above them, even when the window is deactivated. To add or remove this style, use the SetWindowPos function.
  (:WS_EX_TRANSPARENT		#X00000020)	;The window should not be painted until siblings beneath the window (that were created by the same thread) have been painted. The window appears transparent because the bits of underlying sibling windows have already been painted. To achieve transparency without these restrictions, use the SetWindowRgn function.
  (:WS_EX_WINDOWEDGE		#X00000100)	;The window has a border with a raised edge.
  )

(defparameter +WS_EX_OVERLAPPEDWINDOW+          ;The window is an overlapped window.
  '(:WS_EX_WINDOWEDGE
    :WS_EX_CLIENTEDGE))

(defparameter +WS_EX_PALETTEWINDOW+             ;The window is palette window, which is a modeless dialog box that presents an array of commands.
  '(:WS_EX_WINDOWEDGE
    :WS_EX_TOOLWINDOW
    :WS_EX_TOPMOST))

;;; Window Style
(defbitfield (WS_FLAG DWORD)
  (:WS_BORDER		#x00800000)	;The window has a thin-line border.
  (:WS_CAPTION		#x00C00000)	;The window has a title bar (includes the WS_BORDER style).
  (:WS_CHILD		#x40000000)	;The window is a child window. A window with this style cannot have a menu bar. This style cannot be used with the WS_POPUP style.
  (:WS_CHILDWINDOW	#x40000000)	;Same as the WS_CHILD style.
  (:WS_CLIPCHILDREN	#x02000000)	;Excludes the area occupied by child windows when drawing occurs within the parent window. This style is used when creating the parent window.
  (:WS_CLIPSIBLINGS	#x04000000)	;Clips child windows relative to each other; that is, when a particular child window receives a WM_PAINT message, the WS_CLIPSIBLINGS style clips all other overlapping child windows out of the region of the child window to be updated. If WS_CLIPSIBLINGS is not specified and child windows overlap, it is possible, when drawing within the client area of a child window, to draw within the client area of a neighboring child window.
  (:WS_DISABLED		#x08000000)	;The window is initially disabled. A disabled window cannot receive input from the user. To change this after a window has been created, use the EnableWindow function.
  (:WS_DLGFRAME		#x00400000)	;The window has a border of a style typically used with dialog boxes. A window with this style cannot have a title bar.
  (:WS_GROUP		#x00020000)	;The window is the first control of a group of controls. The group consists of this first control and all controls defined after it, up to the next control with the WS_GROUP style. The first control in each group usually has the WS_TABSTOP style so that the user can move from group to group. The user can subsequently change the keyboard focus from one control in the group to the next control in the group by using the direction keys.You can turn this style on and off to change dialog box navigation. To change this style after a window has been created, use the SetWindowLong function.
  (:WS_HSCROLL		#x00100000)     ;The window has a horizontal scroll bar.
  (:WS_ICONIC		#x20000000)	;The window is initially minimized. Same as the WS_MINIMIZE style.
  (:WS_MAXIMIZE		#x01000000)	;The window is initially maximized.
  (:WS_MAXIMIZEBOX	#x00010000)	;The window has a maximize button. Cannot be combined with the WS_EX_CONTEXTHELP style. The WS_SYSMENU style must also be specified.
  (:WS_MINIMIZE		#x20000000)	;The window is initially minimized. Same as the WS_ICONIC style.
  (:WS_MINIMIZEBOX	#x00020000)	;The window has a minimize button. Cannot be combined with the WS_EX_CONTEXTHELP style. The WS_SYSMENU style must also be specified.
  (:WS_OVERLAPPED	#x00000000)	;The window is an overlapped window. An overlapped window has a title bar and a border. Same as the WS_TILED style.
  (:WS_POPUP		#x80000000)	;The windows is a pop-up window. This style cannot be used with the WS_CHILD style.
  (:WS_SIZEBOX		#x00040000)	;The window has a sizing border. Same as the WS_THICKFRAME style.
  (:WS_SYSMENU		#x00080000)	;The window has a window menu on its title bar. The WS_CAPTION style must also be specified.
  (:WS_TABSTOP		#x00010000)	;The window is a control that can receive the keyboard focus when the user presses the TAB key. Pressing the TAB key changes the keyboard focus to the next control with the WS_TABSTOP style.You can turn this style on and off to change dialog box navigation. To change this style after a window has been created, use the SetWindowLong function. For user-created windows and modeless dialogs to work with tab stops, alter the message loop to call the IsDialogMessage function.
  (:WS_THICKFRAME	#x00040000)	;The window has a sizing border. Same as the WS_SIZEBOX style.
  (:WS_TILED		#x00000000)	;The window is an overlapped window. An overlapped window has a title bar and a border. Same as the WS_OVERLAPPED style.
  (:WS_VISIBLE		#x10000000)	;The window is initially visible.This style can be turned on and off by using the ShowWindow or SetWindowPos function.
  (:WS_VSCROLL		#x00200000))	;The window has a vertical scroll bar.

(defparameter +WS_OVERLAPPEDWINDOW+     ;The window is an overlapped window. Same as the WS_TILEDWINDOW style.
  '(:WS_OVERLAPPED
    :WS_CAPTION
    :WS_SYSMENU
    :WS_THICKFRAME
    :WS_MINIMIZEBOX
    :WS_MAXIMIZEBOX))

(defparameter +WS_POPUPWINDOW+          ;The window is a pop-up window. The WS_CAPTION and WS_POPUPWINDOW styles must be combined to make the window menu visible.
  '(:WS_POPUP
    :WS_BORDER
    :WS_SYSMENU))

(defparameter +WS_TILEDWINDOW+;The window is an overlapped window. Same as the WS_OVERLAPPEDWINDOW style.
  '(:WS_OVERLAPPED
    :WS_CAPTION
    :WS_SYSMENU
    :WS_THICKFRAME
    :WS_MINIMIZEBOX
    :WS_MAXIMIZEBOX))

;;; GetModuleHandleEx dwFlags
(defbitfield (GET_MODULE_HANDLE_EX_FLAG DWORD)				;This parameter can be zero or one or more of the following values. If the module's reference count is incremented, the caller must use the FreeLibrary function to decrement the reference count when the module handle is no longer needed.
  (:GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS		#x00000004)	;The lpModuleName parameter is an address in the module.
  (:GET_MODULE_HANDLE_EX_FLAG_PIN			#x00000001)	;The module stays loaded until the process is terminated, no matter how many times FreeLibrary is called.This option cannot be used with GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT.
  (:GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT	#x00000002)	;The reference count for the module is not incremented. This option is equivalent to the behavior of GetModuleHandle. Do not pass the retrieved module handle to the FreeLibrary function; doing so can cause the DLL to be unmapped prematurely. For more information, see Remarks. This option cannot be used with GET_MODULE_HANDLE_EX_FLAG_PIN.
  )

(defcenum (GW_ENUM :uint)
  (:GW_CHILD		5)	;The retrieved handle identifies the child window at the top of the Z order, if the specified window is a parent window; otherwise, the retrieved handle is NULL. The function examines only child windows of the specified window. It does not examine descendant windows.
  (:GW_ENABLEDPOPUP	6)	;The retrieved handle identifies the enabled popup window owned by the specified window (the search uses the first such window found using GW_HWNDNEXT); otherwise, if there are no enabled popup windows, the retrieved handle is that of the specified window.
  (:GW_HWNDFIRST	0)	;The retrieved handle identifies the window of the same type that is highest in the Z order.If the specified window is a topmost window, the handle identifies a topmost window. If the specified window is a top-level window, the handle identifies a top-level window. If the specified window is a child window, the handle identifies a sibling window.
  (:GW_HWNDLAST		1)	;The retrieved handle identifies the window of the same type that is lowest in the Z order.If the specified window is a topmost window, the handle identifies a topmost window. If the specified window is a top-level window, the handle identifies a top-level window. If the specified window is a child window, the handle identifies a sibling window.
  (:GW_HWNDNEXT		2)	;The retrieved handle identifies the window below the specified window in the Z order.If the specified window is a topmost window, the handle identifies a topmost window. If the specified window is a top-level window, the handle identifies a top-level window. If the specified window is a child window, the handle identifies a sibling window.
  (:GW_HWNDPREV		3)	;The retrieved handle identifies the window above the specified window in the Z order.If the specified window is a topmost window, the handle identifies a topmost window. If the specified window is a top-level window, the handle identifies a top-level window. If the specified window is a child window, the handle identifies a sibling window.
  (:GW_OWNER		4))	;The retrieved handle identifies the specified window's owner window, if any. For more information, see Owned Windows.

;;; ShowWindow nCmdShow
(defcenum (SW_ENUM :int)
  (:SW_FORCEMINIMIZE	11)	;Minimizes a window, even if the thread that owns the window is not responding. This flag should only be used when minimizing windows from a different thread.
  (:SW_HIDE		0)	;Hides the window and activates another
  (:SW_MAXIMIZE		3)	;Maximizes the specified window.
  (:SW_MINIMIZE		6)	;Minimizes the specified window and activates the next top-level window in the Z order.
  (:SW_RESTORE		9)	;Activates and displays the window. If the window is minimized or maximized, the system restores it to its original size and position. An application should specify this flag when restoring a minimized window.
  (:SW_SHOW		5)	;Activates the window and displays it in its current size and position.
  (:SW_SHOWDEFAULT	10)	;Sets the show state based on the SW_ value specified in the STARTUPINFO structure passed to the CreateProcess function by the program that started the application.
  (:SW_SHOWMAXIMIZED	3)	;Activates the window and displays it as a maximized window.
  (:SW_SHOWMINIMIZED	2)	;Activates the window and displays it as a minimized window.
  (:SW_SHOWMINNOACTIVE	7)	;Displays the window as a minimized window. This value is similar to SW_SHOWMINIMIZED, except the window is not activated.
  (:SW_SHOWNA		8)	;Displays the window in its current size and position. This value is similar to SW_SHOW, except that the window is not activated.
  (:SW_SHOWNOACTIVATE	4)	;Displays a window in its most recent size and position. This value is similar to SW_SHOWNORMAL, except that the window is not activated.
  (:SW_SHOWNORMAL	1))	;Activates and displays a window. If the window is minimized or maximized, the system restores it to its original size and position. An application should specify this flag when displaying the window for the first

(defbitfield (AW_FLAG DWORD)
  (:AW_ACTIVATE		#x00020000)	;Activates the window. Do not use this value with AW_HIDE.
  (:AW_BLEND		#x00080000)	;Uses a fade effect. This flag can be used only if hwnd is a top-level window.
  (:AW_CENTER		#x00000010)	;Makes the window appear to collapse inward if AW_HIDE is used or expand outward if the AW_HIDE is not used. The various direction flags have no effect.
  (:AW_HIDE		#x00010000)	;Hides the window. By default, the window is shown.
  (:AW_HOR_POSITIVE	#x00000001)	;Animates the window from left to right. This flag can be used with roll or slide animation. It is ignored when used with AW_CENTER or AW_BLEND.
  (:AW_HOR_NEGATIVE	#x00000002)	;Animates the window from right to left. This flag can be used with roll or slide animation. It is ignored when used with AW_CENTER or AW_BLEND.
  (:AW_SLIDE		#x00040000)	;Uses slide animation. By default, roll animation is used. This flag is ignored when used with AW_CENTER.
  (:AW_VER_POSITIVE	#x00000004)	;Animates the window from top to bottom. This flag can be used with roll or slide animation. It is ignored when used with AW_CENTER or AW_BLEND.
  (:AW_VER_NEGATIVE	#x00000008)	;Animates the window from bottom to top. This flag can be used with roll or slide animation. It is ignored when used with AW_CENTER or AW_BLEND.
  )

;;; SetWindowLongPtr nIndex
(defcenum (GWLP_ENUM :int)
  (:GWL_EXSTYLE		-20)	;Sets a new extended window style.
  (:GWLP_HINSTANCE	-6)	;Sets a new application instance handle.
  (:GWLP_ID		-12)	;Sets a new identifier of the child window. The window cannot be a top-level window.
  (:GWL_STYLE		-16)	;Sets a new window style.
  (:GWLP_USERDATA	-21)	;Sets the user data associated with the window. This data is intended for use by the application that created the window. Its value is initially zero.
  (:GWLP_WNDPROC	-4))	;Sets a new address for the window procedure.

(defparameter +DWLP_MSGRESULT+ 0)	;Sets the return value of a message processed in the dialog box procedure.

(defparameter +DWLP_DLGPROC+
  (+ +DWLP_MSGRESULT+
     (foreign-type-size :pointer)))	;Sets the new pointer to the dialog box procedure.

(defparameter +DWLP_USER+
  (+ +DWLP_DLGPROC+
     (foreign-type-size :pointer)))

(defcstruct ACCEL
  (:fVirt C_BYTE)
  (:key   WORD)
  (:cmd   WORD))

(defbitfield (ACCEL_VIRT_FLAG C_BYTE)
  (:ACCEL_VIRT_ALT	#x10)	;The ALT key must be held down when the accelerator key is pressed.
  (:ACCEL_VIRT_CONTROL	#x08)	;The CTRL key must be held down when the accelerator key is pressed.
  (:ACCEL_VIRT_NOINVERT #x02)	;No top-level menu item is highlighted when the accelerator is used. If this flag is not specified, a top-level menu item will be highlighted, if possible, when the accelerator is used. This attribute is obsolete and retained only for backward compatibility with resource files designed for 16-bit Windows.
  (:ACCEL_VIRT_SHIFT	#x04)	;The SHIFT key must be held down when the accelerator key is pressed.
  (:ACCEL_VIRT_VIRTKEY	#x01)	;The key member specifies a virtual-key code. If this flag is not specified, key is assumed to specify a character code.
  )

(defparameter +HWND_BROADCAST+ #xffff);The message is posted to all top-level windows in the system, including disabled or invisible unowned windows, overlapped windows, and pop-up windows. The message is not posted to child windows.

(defcenum (WM_ENUM :uint)
  (:WM_NULL						#x0000)
  (:WM_CREATE						#x0001)
  (:WM_DESTROY						#x0002)
  (:WM_MOVE						#x0003)
  (:WM_SIZE						#x0005)
  (:WM_ACTIVATE						#x0006)
  (:WM_SETFOCUS						#x0007)
  (:WM_KILLFOCUS					#x0008)
  (:WM_ENABLE						#x000A)
  (:WM_SETREDRAW					#x000B)
  (:WM_SETTEXT						#x000C)
  (:WM_GETTEXT						#x000D)
  (:WM_GETTEXTLENGTH					#x000E)
  (:WM_PAINT						#x000F)
  (:WM_CLOSE						#x0010)
  (:WM_QUERYENDSESSION					#x0011)
  (:WM_QUERYOPEN					#x0013)
  (:WM_ENDSESSION					#x0016)
  (:WM_QUIT						#x0012)
  (:WM_ERASEBKGND					#x0014)
  (:WM_SYSCOLORCHANGE					#x0015)
  (:WM_SHOWWINDOW					#x0018)
  (:WM_SETTINGCHANGE					#x001A)
  (:WM_DEVMODECHANGE					#x001B)
  (:WM_ACTIVATEAPP					#x001C)
  (:WM_FONTCHANGE					#x001D)
  (:WM_TIMECHANGE					#x001E)
  (:WM_CANCELMODE					#x001F)
  (:WM_SETCURSOR					#x0020)
  (:WM_MOUSEACTIVATE					#x0021)
  (:WM_CHILDACTIVATE					#x0022)
  (:WM_QUEUESYNC					#x0023)
  (:WM_GETMINMAXINFO					#x0024)
  (:WM_PAINTICON					#x0026)
  (:WM_ICONERASEBKGND					#x0027)
  (:WM_NEXTDLGCTL					#x0028)
  (:WM_SPOOLERSTATUS					#x002A)
  (:WM_DRAWITEM						#x002B)
  (:WM_MEASUREITEM					#x002C)
  (:WM_DELETEITEM					#x002D)
  (:WM_VKEYTOITEM					#x002E)
  (:WM_CHARTOITEM					#x002F)
  (:WM_SETFONT						#x0030)
  (:WM_GETFONT						#x0031)
  (:WM_SETHOTKEY					#x0032)
  (:WM_GETHOTKEY					#x0033)
  (:WM_QUERYDRAGICON					#x0037)
  (:WM_COMPAREITEM					#x0039)
  (:WM_GETOBJECT					#x003D)
  (:WM_COMPACTING					#x0041)
  (:WM_COMMNOTIFY					#x0044) ;no longer suported
  (:WM_WINDOWPOSCHANGING				#x0046)
  (:WM_WINDOWPOSCHANGED					#x0047)
  (:WM_POWER						#x0048)
  (:WM_COPYDATA						#x004A)
  (:WM_CANCELJOURNAL					#x004B)
  (:WM_NOTIFY						#x004E)
  (:WM_INPUTLANGCHANGEREQUEST				#x0050)
  (:WM_INPUTLANGCHANGE					#x0051)
  (:WM_TCARD						#x0052)
  (:WM_HELP						#x0053)
  (:WM_USERCHANGED					#x0054)
  (:WM_NOTIFYFORMAT					#x0055)
  (:WM_CONTEXTMENU					#x007B)
  (:WM_STYLECHANGING					#x007C)
  (:WM_STYLECHANGED					#x007D)
  (:WM_DISPLAYCHANGE					#x007E)
  (:WM_GETICON						#x007F)
  (:WM_SETICON						#x0080)
  (:WM_NCCREATE						#x0081)
  (:WM_NCDESTROY					#x0082)
  (:WM_NCCALCSIZE					#x0083)
  (:WM_NCHITTEST					#x0084)
  (:WM_NCPAINT						#x0085)
  (:WM_NCACTIVATE					#x0086)
  (:WM_GETDLGCODE					#x0087)
  (:WM_SYNCPAINT					#x0088)
  (:WM_NCMOUSEMOVE					#x00A0)
  (:WM_NCLBUTTONDOWN					#x00A1)
  (:WM_NCLBUTTONUP					#x00A2)
  (:WM_NCLBUTTONDBLCLK					#x00A3)
  (:WM_NCRBUTTONDOWN					#x00A4)
  (:WM_NCRBUTTONUP					#x00A5)
  (:WM_NCRBUTTONDBLCLK					#x00A6)
  (:WM_NCMBUTTONDOWN					#x00A7)
  (:WM_NCMBUTTONUP					#x00A8)
  (:WM_NCMBUTTONDBLCLK					#x00A9)
  (:WM_NCXBUTTONDOWN					#x00AB)
  (:WM_NCXBUTTONUP					#x00AC)
  (:WM_NCXBUTTONDBLCLK					#x00AD)
  (:WM_INPUT_DEVICE_CHANGE				#x00FE)
  (:WM_INPUT						#x00FF)
  (:WM_KEYFIRST						#x0100)
  (:WM_KEYDOWN						#x0100)
  (:WM_KEYUP						#x0101)
  (:WM_CHAR						#x0102)
  (:WM_DEADCHAR						#x0103)
  (:WM_SYSKEYDOWN					#x0104)
  (:WM_SYSKEYUP						#x0105)
  (:WM_SYSCHAR						#x0106)
  (:WM_SYSDEADCHAR					#x0107)
  (:WM_UNICHAR						#x0109)
  (:WM_KEYLAST						#x0109)
					;  (:KEYLAST    #x0108)
  (:WM_IME_STARTCOMPOSITION				#x010D)
  (:WM_IME_ENDCOMPOSITION				#x010E)
  (:WM_IME_COMPOSITION					#x010F)
  (:WM_IME_KEYLAST					#x010F)
  (:WM_INITDIALOG					#x0110)
  (:WM_COMMAND						#x0111)
  (:WM_SYSCOMMAND					#x0112)
  (:WM_TIMER						#x0113)
  (:WM_HSCROLL						#x0114)
  (:WM_VSCROLL						#x0115)
  (:WM_INITMENU						#x0116)
  (:WM_INITMENUPOPUP					#x0117)
  (:WM_GESTURE						#x0119)
  (:WM_GESTURENOTIFY					#x011A)
  (:WM_MENUSELECT					#x011F)
  (:WM_MENUCHAR						#x0120)
  (:WM_ENTERIDLE					#x0121)
  (:WM_MENURBUTTONUP					#x0122)
  (:WM_MENUDRAG						#x0123)
  (:WM_MENUGETOBJECT					#x0124)
  (:WM_UNINITMENUPOPUP					#x0125)
  (:WM_MENUCOMMAND					#x0126)
  (:WM_CHANGEUISTATE					#x0127)
  (:WM_UPDATEUISTATE					#x0128)
  (:WM_QUERYUISTATE					#x0129)
  (:WM_CTLCOLORMSGBOX					#x0132)
  (:WM_CTLCOLOREDIT					#x0133)
  (:WM_CTLCOLORLISTBOX					#x0134)
  (:WM_CTLCOLORBTN					#x0135)
  (:WM_CTLCOLORDLG					#x0136)
  (:WM_CTLCOLORSCROLLBAR				#x0137)
  (:WM_CTLCOLORSTATIC					#x0138)
  (:WM_MOUSEFIRST					#x0200)
  (:WM_MOUSEMOVE					#x0200)
  (:WM_LBUTTONDOWN					#x0201)
  (:WM_LBUTTONUP					#x0202)
  (:WM_LBUTTONDBLCLK					#x0203)
  (:WM_RBUTTONDOWN					#x0204)
  (:WM_RBUTTONUP					#x0205)
  (:WM_RBUTTONDBLCLK					#x0206)
  (:WM_MBUTTONDOWN					#x0207)
  (:WM_MBUTTONUP					#x0208)
  (:WM_MBUTTONDBLCLK					#x0209)
  (:WM_MOUSEWHEEL					#x020A)
  (:WM_XBUTTONDOWN					#x020B)
  (:WM_XBUTTONUP					#x020C)
  (:WM_XBUTTONDBLCLK					#x020D)
  (:WM_MOUSEHWHEEL					#x020E)
  (:WM_MOUSELAST					#x020E)
					;  (:MOUSELAST  #x020D)
					;  (:MOUSELAST  #x020A)
					;  (:MOUSELAST  #x0209)
  (:WM_PARENTNOTIFY					#x0210)
  (:WM_ENTERMENULOOP					#x0211)
  (:WM_EXITMENULOOP					#x0212)
  (:WM_NEXTMENU						#x0213)
  (:WM_SIZING						#x0214)
  (:WM_CAPTURECHANGED					#x0215)
  (:WM_MOVING						#x0216)
  (:WM_POWERBROADCAST					#x0218)
  (:WM_DEVICECHANGE					#x0219)
  (:WM_MDICREATE					#x0220)
  (:WM_MDIDESTROY					#x0221)
  (:WM_MDIACTIVATE					#x0222)
  (:WM_MDIRESTORE					#x0223)
  (:WM_MDINEXT						#x0224)
  (:WM_MDIMAXIMIZE					#x0225)
  (:WM_MDITILE						#x0226)
  (:WM_MDICASCADE					#x0227)
  (:WM_MDIICONARRANGE					#x0228)
  (:WM_MDIGETACTIVE					#x0229)
  (:WM_MDISETMENU					#x0230)
  (:WM_ENTERSIZEMOVE					#x0231)
  (:WM_EXITSIZEMOVE					#x0232)
  (:WM_DROPFILES					#x0233)
  (:WM_MDIREFRESHMENU					#x0234)
  (:WM_POINTERDEVICECHANGE				#x238)
  (:WM_POINTERDEVICEINRANGE				#x239)
  (:WM_POINTERDEVICEOUTOFRANGE				#x23A)
  (:WM_TOUCH						#x0240)
  (:WM_NCPOINTERUPDATE					#x0241)
  (:WM_NCPOINTERDOWN					#x0242)
  (:WM_NCPOINTERUP					#x0243)
  (:WM_POINTERUPDATE					#x0245)
  (:WM_POINTERDOWN					#x0246)
  (:WM_POINTERUP					#x0247)
  (:WM_POINTERENTER					#x0249)
  (:WM_POINTERLEAVE					#x024A)
  (:WM_POINTERACTIVATE					#x024B)
  (:WM_POINTERCAPTURECHANGED				#x024C)
  (:WM_TOUCHHITTESTING					#x024D)
  (:WM_POINTERWHEEL					#x024E)
  (:WM_POINTERHWHEEL					#x024F)
  (:WM_IME_SETCONTEXT					#x0281)
  (:WM_IME_NOTIFY					#x0282)
  (:WM_IME_CONTROL					#x0283)
  (:WM_IME_COMPOSITIONFULL				#x0284)
  (:WM_IME_SELECT					#x0285)
  (:WM_IME_CHAR						#x0286)
  (:WM_IME_REQUEST					#x0288)
  (:WM_IME_KEYDOWN					#x0290)
  (:WM_IME_KEYUP					#x0291)
  (:WM_MOUSEHOVER					#x02A1)
  (:WM_MOUSELEAVE					#x02A3)
  (:WM_NCMOUSEHOVER					#x02A0)
  (:WM_NCMOUSELEAVE					#x02A2)
  (:WM_WTSSESSION_CHANGE				#x02B1)
  (:WM_TABLET_FIRST					#x02c0)
  (:WM_TABLET_LAST					#x02df)
  (:WM_DPICHANGED					#x02E0)
  (:WM_CUT						#x0300)
  (:WM_COPY						#x0301)
  (:WM_PASTE						#x0302)
  (:WM_CLEAR						#x0303)
  (:WM_UNDO						#x0304)
  (:WM_RENDERFORMAT					#x0305)
  (:WM_RENDERALLFORMATS					#x0306)
  (:WM_DESTROYCLIPBOARD					#x0307)
  (:WM_DRAWCLIPBOARD					#x0308)
  (:WM_PAINTCLIPBOARD					#x0309)
  (:WM_VSCROLLCLIPBOARD					#x030A)
  (:WM_SIZECLIPBOARD					#x030B)
  (:WM_ASKCBFORMATNAME					#x030C)
  (:WM_CHANGECBCHAIN					#x030D)
  (:WM_HSCROLLCLIPBOARD					#x030E)
  (:WM_QUERYNEWPALETTE					#x030F)
  (:WM_PALETTEISCHANGING				#x0310)
  (:WM_PALETTECHANGED					#x0311)
  (:WM_HOTKEY						#x0312)
  (:WM_PRINT						#x0317)
  (:WM_PRINTCLIENT					#x0318)
  (:WM_APPCOMMAND					#x0319)
  (:WM_THEMECHANGED					#x031A)
  (:WM_CLIPBOARDUPDATE					#x031D)
  (:WM_DWMCOMPOSITIONCHANGED				#x031E)
  (:WM_DWMNCRENDERINGCHANGED				#x031F)
  (:WM_DWMCOLORIZATIONCOLORCHANGED			#x0320)
  (:WM_DWMWINDOWMAXIMIZEDCHANGE				#x0321)
  (:WM_DWMSENDICONICTHUMBNAIL				#x0323)
  (:WM_DWMSENDICONICLIVEPREVIEWBITMAP			#x0326)
  (:WM_GETTITLEBARINFOEX				#x033F)
  (:WM_HANDHELDFIRST					#x0358)
  (:WM_HANDHELDLAST					#x035F)
  (:WM_AFXFIRST						#x0360)
  (:WM_AFXLAST						#x037F)
  (:WM_PENWINFIRST					#x0380)
  (:WM_PENWINLAST					#x038F)
  (:WM_APP						#x8000)
  (:WM_USER						#x0400)
  )

(defcenum (BM_ENUM :uint)
  (:BM_GETCHECK        #x00F0)
  (:BM_SETCHECK        #x00F1)
  (:BM_GETSTATE        #x00F2)
  (:BM_SETSTATE        #x00F3)
  (:BM_SETSTYLE        #x00F4)
					;(WINVER >= 0x0400)
  (:BM_CLICK           #x00F5)
  (:BM_GETIMAGE        #x00F6)
  (:BM_SETIMAGE        #x00F7)
					;(WINVER >= 0x0600)
  (:BM_SETDONTCLICK    #x00F8))

(defcenum (BN_ENUM WORD)
  (:BN_CLICKED          0)
  (:BN_PAINT            1)
  (:BN_HILITE           2)
  (:BN_UNHILITE         3)
  (:BN_DISABLE          4)
  (:BN_DOUBLECLICKED    5)
					;WINVER >= 0x0400
  (:BN_PUSHED           2)
  (:BN_UNPUSHED         3)
  (:BN_DBLCLK           5)
  (:BN_SETFOCUS         6)
  (:BN_KILLFOCUS        7))

(defcenum (USERM_ENUM :uint)
  (:UM_EVAL  #x0401))

(defctype WND_MESSAGE (enum-union :uint WM_ENUM BM_ENUM USERM_ENUM))

(defcstruct POINT
  (:x :long)
  (:y :long))

(defcstruct MSG
  (:hwnd    HWND)
  (:message WND_MESSAGE)
  (:wParam  WPARAM)
  (:lParam  LPARAM)
  (:time    DWORD)
  (:pt      (:struct POINT)))

(defcenum (GA_ENUM :uint)
  (:GA_PARENT		1);Retrieves the parent window. This does not include the owner, as it does with the GetParent function.
  (:GA_ROOT		2);Retrieves the root window by walking the chain of parent windows.
  (:GA_ROOTOWNER	3);Retrieves the owned root window by walking the chain of parent and owner windows returned by GetParent.
  )

;;;
(defcstruct RECT
  (:left	:long)
  (:top		:long)
  (:right	:long)
  (:bottom	:long))

(defcstruct PAINTSTRUCT
  (:hdc         HDC)
  (:fErase      :boolean)
  (:rcPaint     (:struct RECT))
  (:fRestore    :boolean)
  (:fIncUpdate  :boolean)
  (:rgbReserved C_BYTE :count 32))

(defcstruct POINTL
  (:x :long)
  (:y :long))

(defcstruct DM_DISPLAY
  (:dmPosition (:struct POINTL))
  (:dmDisplayOrientation  DWORD)
  (:dmDisplayFixedOutput  DWORD))

(defcstruct DM_PRINTER
  (:dmOrientation	:short)
  (:dmPaperSize		:short)
  (:dmPaperLength	:short)
  (:dmPaperWidth	:short)
  (:dmScale		:short)
  (:dmCopies		:short)
  (:dmDefaultSource	:short)
  (:dmPrintQuality	:short))

(defcunion DM_DEVICE
  (:dmPrinter (:struct DM_PRINTER))
  (:dmDisplay (:struct DM_DISPLAY)))

(defcunion DM_MISC
  (:dmDisplayFlags	DWORD)
  (:dmNup		DWORD))

(defcstruct DEVMODE
  (:dmDeviceName	WORD :count 32)		;wchar_t[32]
  (:dmSpecVersion	WORD)
  (:dmDriverVersion	WORD)
  (:dmSize		WORD)
  (:dmDriverExtra	WORD)
  (:dmFields           DWORD)
  (:dmDevice (:union DM_DEVICE))
  (:dmColor		:short)
  (:dmDuplex		:short)
  (:dmYResolution	:short)
  (:dmTTOption		:short)
  (:dmCollate		:short)
  (:dmFormName	WORD :count 32)		;wchar_t[32]
  (:dmLogPixels WORD)
  (:dmBitsPerPel	DWORD)
  (:dmPelsWidth		DWORD)
  (:dmPelsHeight	DWORD)
  (:dmMisc   (:union DM_MISC))
  (:dmDisplayFrequency  DWORD)
  ;; (WINVER >= 0x0400)
  (:dmICMMethod		DWORD)
  (:dmICMIntent		DWORD)
  (:dmMediaType		DWORD)
  (:dmDitherType	DWORD)
  (:dmReserved1		DWORD)
  (:dmReserved2		DWORD)
  ;; (WINVER >= 0x0500) || (_WIN32_WINNT >= 0x0400)
  (:dmPanningWidth	DWORD)
  (:dmPanningHeight	DWORD))

(defbitfield (CWP_FLAG :uint)
  (:CWP_ALL		#x0000)		;Does not skip any child windows
  (:CWP_SKIPDISABLED	#x0002)		;Skips disabled child windows
  (:CWP_SKIPINVISIBLE	#x0001)		;Skips invisible child windows
  (:CWP_SKIPTRANSPARENT #x0004))	;Skips transparent child windows

;;; Kernel32
(defcenum (PROCESSOR_ARCHITECTURE_ENUM WORD)
  (:PROCESSOR_ARCHITECTURE_INTEL            0)
  (:PROCESSOR_ARCHITECTURE_MIPS             1)
  (:PROCESSOR_ARCHITECTURE_ALPHA            2)
  (:PROCESSOR_ARCHITECTURE_PPC              3)
  (:PROCESSOR_ARCHITECTURE_SHX              4)
  (:PROCESSOR_ARCHITECTURE_ARM              5)
  (:PROCESSOR_ARCHITECTURE_IA64             6)
  (:PROCESSOR_ARCHITECTURE_ALPHA64          7)
  (:PROCESSOR_ARCHITECTURE_MSIL             8)
  (:PROCESSOR_ARCHITECTURE_AMD64            9)
  (:PROCESSOR_ARCHITECTURE_IA32_ON_WIN64    10)
  (:PROCESSOR_ARCHITECTURE_NEUTRAL          11)
  (:PROCESSOR_ARCHITECTURE_ARM64            12)
  (:PROCESSOR_ARCHITECTURE_UNKNOWN          #xFFFF))

(defcenum (PROCESSOR_ENUM DWORD)
  (:PROCESSOR_INTEL_386     386)
  (:PROCESSOR_INTEL_486     486)
  (:PROCESSOR_INTEL_PENTIUM 586)
  (:PROCESSOR_INTEL_IA64    2200)
  (:PROCESSOR_AMD_X8664     8664)
  (:PROCESSOR_MIPS_R4000    4000)  
  (:PROCESSOR_ALPHA_21064   21064)
  (:PROCESSOR_PPC_601       601)
  (:PROCESSOR_PPC_603       603)
  (:PROCESSOR_PPC_604       604)
  (:PROCESSOR_PPC_620       620)
  (:PROCESSOR_HITACHI_SH3   10003) 
  (:PROCESSOR_HITACHI_SH3E  10004) 
  (:PROCESSOR_HITACHI_SH4   10005) 
  (:PROCESSOR_MOTOROLA_821  821)   
  (:PROCESSOR_SHx_SH3       103)   
  (:PROCESSOR_SHx_SH4       104)   
  (:PROCESSOR_STRONGARM     2577)  
  (:PROCESSOR_ARM720        1824)  
  (:PROCESSOR_ARM820        2080)  
  (:PROCESSOR_ARM920        2336)  
  (:PROCESSOR_ARM_7TDMI     70001) 
  (:PROCESSOR_OPTIL         #x494f))

(defcstruct SYSTEM_INFO
  (:wProcessorArchitecture	PROCESSOR_ARCHITECTURE_ENUM)
  (:wReserved			WORD)
  (:dwPageSize			DWORD)
  (:lpMinimumApplicationAddress LPVOID)
  (:lpMaximumApplicationAddress LPVOID)
  (:dwActiveProcessorMask	DWORD_PTR)
  (:dwNumberOfProcessors	DWORD)
  (:dwProcessorType		PROCESSOR_ENUM)
  (:dwAllocationGranularity     DWORD)
  (:wProcessorLevel             WORD)
  (:wProcessorRevision          WORD))

(defcenum (FIRMWARE_TYPE_ENUM :uint)
  (:FirmwareTypeUnknown  0)
  (:FirmwareTypeBios     1)
  (:FirmwareTypeUefi     2)
  (:FirmwareTypeMax      3))

(defbitfield (FORMAT_MESSAGE_FLAG DWORD)
  (:FORMAT_MESSAGE_ALLOCATE_BUFFER	#x00000100)
  (:FORMAT_MESSAGE_ARGUMENT_ARRAY	#x00002000)
  (:FORMAT_MESSAGE_FROM_HMODULE		#x00000800)
  (:FORMAT_MESSAGE_FROM_STRING		#x00000400)
  (:FORMAT_MESSAGE_FROM_SYSTEM		#x00001000)
  (:FORMAT_MESSAGE_IGNORE_INSERTS	#x00000200))

(defbitfield (VER_SUITE_FLAG WORD)
  (:VER_SUITE_BACKOFFICE		#x00000004)	;Microsoft BackOffice components are installed.
  (:VER_SUITE_BLADE			#x00000400)	;Windows Server 2003, Web Edition is installed.
  (:VER_SUITE_COMPUTE_SERVER		#x00004000)	;Windows Server 2003, Compute Cluster Edition is installed.
  (:VER_SUITE_DATACENTER		#x00000080)	;Windows Server 2008 Datacenter, Windows Server 2003, Datacenter Edition, or Windows 2000 Datacenter Server is installed.
  (:VER_SUITE_ENTERPRISE		#x00000002)	;Windows Server 2008 Enterprise, Windows Server 2003, Enterprise Edition, or Windows 2000 Advanced Server is installed. Refer to the Remarks section for more information about this bit flag.
  (:VER_SUITE_EMBEDDEDNT		#x00000040)	;Windows XP Embedded is installed.
  (:VER_SUITE_PERSONAL			#x00000200)	;Windows Vista Home Premium, Windows Vista Home Basic, or Windows XP Home Edition is installed.
  (:VER_SUITE_SINGLEUSERTS		#x00000100)	;Remote Desktop is supported, but only one interactive session is supported. This value is set unless the system is running in application server mode.
  (:VER_SUITE_SMALLBUSINESS		#x00000001)	;Microsoft Small Business Server was once installed on the system, but may have been upgraded to another version of Windows. Refer to the Remarks section for more information about this bit flag.
  (:VER_SUITE_SMALLBUSINESS_RESTRICTED	#x00000020)	;Microsoft Small Business Server is installed with the restrictive client license in force. Refer to the Remarks section for more information about this bit flag.
  (:VER_SUITE_STORAGE_SERVER		#x00002000)	;Windows Storage Server 2003 R2 or Windows Storage Server 2003is installed.
  (:VER_SUITE_TERMINAL			#x00000010)	;Terminal Services is installed. This value is always set.If VER_SUITE_TERMINAL is set but VER_SUITE_SINGLEUSERTS is not set, the system is running in application server mode.
  (:VER_SUITE_WH_SERVER			#x00008000))	;Windows Home Server is installed.

(defbitfield (VER_NT_FLAG C_BYTE)
  (:VER_NT_DOMAIN_CONTROLLER	#x0000002)	;The system is a domain controller and the operating system is Windows Server 2012 , Windows Server 2008 R2, Windows Server 2008, Windows Server 2003, or Windows 2000 Server.
  (:VER_NT_SERVER		#x0000003)	;The operating system is Windows Server 2012, Windows Server 2008 R2, Windows Server 2008, Windows Server 2003, or Windows 2000 Server.Note that a server that is also a domain controller is reported as VER_NT_DOMAIN_CONTROLLER, not VER_NT_SERVER.
  (:VER_NT_WORKSTATION		#x0000001))	;The operating system is Windows 8, Windows 7, Windows Vista, Windows XP Professional, Windows XP Home Edition, or Windows 2000 Professional.

(defcstruct OSVERSIONINFOEX
  (:dwOSVersionInfoSize DWORD)
  (:dwMajorVersion	DWORD)
  (:dwMinorVersion	DWORD)
  (:dwBuildNumber	DWORD)
  (:dwPlatformId	DWORD)
  (:szCSDVersion	WORD :count 128)
  (:wServicePackMajor	WORD)
  (:wServicePackMinor	WORD)
  (:wSuiteMask		VER_SUITE_FLAG)
  (:wProductType	VER_NT_FLAG)
  (:wReserved		C_BYTE))

(defcenum (PRODUCT_ENUM DWORD)
  (:PRODUCT_BUSINESS #x00000006)	   ;Business
  (:PRODUCT_BUSINESS_N #x00000010);Business N
  (:PRODUCT_CLUSTER_SERVER #x00000012);HPC Edition
  (:PRODUCT_CLUSTER_SERVER_V #x00000040);Server Hyper Core V
  (:PRODUCT_CORE #x00000065);Windows 10 Home
  (:PRODUCT_CORE_COUNTRYSPECIFIC #x00000063);Windows 10 Home China
  (:PRODUCT_CORE_N #x00000062);Windows 10 Home N
  (:PRODUCT_CORE_SINGLELANGUAGE #x00000064);Windows 10 Home Single Language
  (:PRODUCT_EDUCATION #x00000079);Windows 10 Education
  (:PRODUCT_EDUCATION_N #x0000007A);Windows 10 Education N
  (:PRODUCT_DATACENTER_EVALUATION_SERVER #x00000050);Server Datacenter (evaluation installation)
  (:PRODUCT_DATACENTER_SERVER #x00000008);Server Datacenter (full installation)
  (:PRODUCT_DATACENTER_SERVER_CORE #x0000000C);Server Datacenter (core installation)
  (:PRODUCT_DATACENTER_SERVER_CORE_V #x00000027);Server Datacenter without Hyper-V (core installation)
  (:PRODUCT_DATACENTER_SERVER_V #x00000025);Server Datacenter without Hyper-V (full installation)
  (:PRODUCT_ENTERPRISE #x00000004);Windows 10 Enterprise
  (:PRODUCT_ENTERPRISE_E #x00000046);Windows 10 Enterprise E
  (:PRODUCT_ENTERPRISE_EVALUATION #x00000048);Windows 10 Enterprise Evaluation
  (:PRODUCT_ENTERPRISE_N #x0000001B);Windows 10 Enterprise N
  (:PRODUCT_ENTERPRISE_N_EVALUATION #x00000054);Windows 10 Enterprise N Evaluation
  (:PRODUCT_ENTERPRISE_S #x0000007D);Windows 10 Enterprise 2015 LTSB
  (:PRODUCT_ENTERPRISE_S_EVALUATION #x00000081);Windows 10 Enterprise 2015 LTSB Evaluation
  (:PRODUCT_ENTERPRISE_S_N #x0000007E);Windows 10 Enterprise 2015 LTSB N
  (:PRODUCT_ENTERPRISE_S_N_EVALUATION #x00000082);Windows 10 Enterprise 2015 LTSB N Evaluation
  (:PRODUCT_ENTERPRISE_SERVER #x0000000A);Server Enterprise (full installation)
  (:PRODUCT_ENTERPRISE_SERVER_CORE #x0000000E);Server Enterprise (core installation)
  (:PRODUCT_ENTERPRISE_SERVER_CORE_V #x00000029);Server Enterprise without Hyper-V (core installation)
  (:PRODUCT_ENTERPRISE_SERVER_IA64 #x0000000F);Server Enterprise for Itanium-based Systems
  (:PRODUCT_ENTERPRISE_SERVER_V #x00000026);Server Enterprise without Hyper-V (full installation)
  (:PRODUCT_ESSENTIALBUSINESS_SERVER_ADDL #x0000003C);Windows Essential Server Solution Additional
  (:PRODUCT_ESSENTIALBUSINESS_SERVER_ADDLSVC #x0000003E);Windows Essential Server Solution Additional SVC
  (:PRODUCT_ESSENTIALBUSINESS_SERVER_MGMT #x0000003B);Windows Essential Server Solution Management
  (:PRODUCT_ESSENTIALBUSINESS_SERVER_MGMTSVC #x0000003D);Windows Essential Server Solution Management SVC
  (:PRODUCT_HOME_BASIC #x00000002);Home Basic
  (:PRODUCT_HOME_BASIC_E #x00000043);Not supported
  (:PRODUCT_HOME_BASIC_N #x00000005);Home Basic N
  (:PRODUCT_HOME_PREMIUM #x00000003);Home Premium
  (:PRODUCT_HOME_PREMIUM_E #x00000044);Not supported
  (:PRODUCT_HOME_PREMIUM_N #x0000001A);Home Premium N
  (:PRODUCT_HOME_PREMIUM_SERVER #x00000022);Windows Home Server 2011
  (:PRODUCT_HOME_SERVER #x00000013);Windows Storage Server 2008 R2 Essentials
  (:PRODUCT_HYPERV #x0000002A);Microsoft Hyper-V Server
  (:PRODUCT_MEDIUMBUSINESS_SERVER_MANAGEMENT #x0000001E);Windows Essential Business Server Management Server
  (:PRODUCT_MEDIUMBUSINESS_SERVER_MESSAGING #x00000020);Windows Essential Business Server Messaging Server
  (:PRODUCT_MEDIUMBUSINESS_SERVER_SECURITY #x0000001F);Windows Essential Business Server Security Server
  (:PRODUCT_MOBILE_CORE #x00000068);Windows 10 Mobile
  (:PRODUCT_MOBILE_ENTERPRISE #x00000085);Windows 10 Mobile Enterprise
  (:PRODUCT_MULTIPOINT_PREMIUM_SERVER #x0000004D);Windows MultiPoint Server Premium (full installation)
  (:PRODUCT_MULTIPOINT_STANDARD_SERVER #x0000004C);Windows MultiPoint Server Standard (full installation)
  (:PRODUCT_PROFESSIONAL #x00000030);Windows 10 Pro
  (:PRODUCT_PROFESSIONAL_E #x00000045);Not supported
  (:PRODUCT_PROFESSIONAL_N #x00000031);Windows 10 Pro N
  (:PRODUCT_PROFESSIONAL_WMC #x00000067);Professional with Media Center
  (:PRODUCT_SB_SOLUTION_SERVER #x00000032);Windows Small Business Server 2011 Essentials
  (:PRODUCT_SB_SOLUTION_SERVER_EM #x00000036);Server For SB Solutions EM
  (:PRODUCT_SERVER_FOR_SB_SOLUTIONS #x00000033);Server For SB Solutions
  (:PRODUCT_SERVER_FOR_SB_SOLUTIONS_EM #x00000037);Server For SB Solutions EM
  (:PRODUCT_SERVER_FOR_SMALLBUSINESS #x00000018);Windows Server 2008 for Windows Essential Server Solutions
  (:PRODUCT_SERVER_FOR_SMALLBUSINESS_V #x00000023);Windows Server 2008 without Hyper-V for Windows Essential Server Solutions
  (:PRODUCT_SERVER_FOUNDATION #x00000021);Server Foundation
  (:PRODUCT_SMALLBUSINESS_SERVER #x00000009);Windows Small Business Server
  (:PRODUCT_SMALLBUSINESS_SERVER_PREMIUM #x00000019);Small Business Server Premium
  (:PRODUCT_SMALLBUSINESS_SERVER_PREMIUM_CORE #x0000003F);Small Business Server Premium (core installation)
  (:PRODUCT_SOLUTION_EMBEDDEDSERVER #x00000038);Windows MultiPoint Server
  (:PRODUCT_STANDARD_EVALUATION_SERVER #x0000004F);Server Standard (evaluation installation)
  (:PRODUCT_STANDARD_SERVER #x00000007);Server Standard
  (:PRODUCT_STANDARD_SERVER_CORE #x0000000D);Server Standard (core installation)
  (:PRODUCT_STANDARD_SERVER_CORE_V #x00000028);Server Standard without Hyper-V (core installation)
  (:PRODUCT_STANDARD_SERVER_V #x00000024);Server Standard without Hyper-V
  (:PRODUCT_STANDARD_SERVER_SOLUTIONS #x00000034);Server Solutions Premium
  (:PRODUCT_STANDARD_SERVER_SOLUTIONS_CORE #x00000035);Server Solutions Premium (core installation)
  (:PRODUCT_STARTER #x0000000B);Starter
  (:PRODUCT_STARTER_E #x00000042);Not supported
  (:PRODUCT_STARTER_N #x0000002F);Starter N
  (:PRODUCT_STORAGE_ENTERPRISE_SERVER #x00000017);Storage Server Enterprise
  (:PRODUCT_STORAGE_ENTERPRISE_SERVER_CORE #x0000002E);Storage Server Enterprise (core installation)
  (:PRODUCT_STORAGE_EXPRESS_SERVER #x00000014);Storage Server Express
  (:PRODUCT_STORAGE_EXPRESS_SERVER_CORE #x0000002B);Storage Server Express (core installation)
  (:PRODUCT_STORAGE_STANDARD_EVALUATION_SERVER #x00000060);Storage Server Standard (evaluation installation)
  (:PRODUCT_STORAGE_STANDARD_SERVER #x00000015);Storage Server Standard
  (:PRODUCT_STORAGE_STANDARD_SERVER_CORE #x0000002C);Storage Server Standard (core installation)
  (:PRODUCT_STORAGE_WORKGROUP_EVALUATION_SERVER #x0000005F);Storage Server Workgroup (evaluation installation)
  (:PRODUCT_STORAGE_WORKGROUP_SERVER #x00000016);Storage Server Workgroup
  (:PRODUCT_STORAGE_WORKGROUP_SERVER_CORE #x0000002D);Storage Server Workgroup (core installation)
  (:PRODUCT_ULTIMATE #x00000001);Ultimate
  (:PRODUCT_ULTIMATE_E #x00000047);Not supported
  (:PRODUCT_ULTIMATE_N #x0000001C);Ultimate N
  (:PRODUCT_UNDEFINED #x00000000);An unknown product
  (:PRODUCT_WEB_SERVER #x00000011);Web Server (full installation)
  (:PRODUCT_WEB_SERVER_CORE #x0000001D));Web Server (core installation)

(defcenum (SM_ENUM :int)
  (:SM_ARRANGE 56)			;The flags that specify how the system arranged minimized windows. For more information, see the Remarks section in this topic.
  (:SM_CLEANBOOT 67)			;The value that specifies how the system is started:
					;0 Normal boot
					;1 Fail-safe boot
					;2 Fail-safe with network boot
					;A fail-safe boot (also called SafeBoot, Safe Mode, or Clean Boot) bypasses the user startup files.
  (:SM_CMONITORS 80)			;The number of display monitors on a desktop. For more information, see the Remarks section in this topic.
  (:SM_CMOUSEBUTTONS 43)		;The number of buttons on a mouse, or zero if no mouse is installed.
  (:SM_CONVERTIBLESLATEMODE #x2003)	;Reflects the state of the laptop or slate mode, 0 for Slate Mode and non-zero otherwise. When this system metric changes, the system sends a broadcast message via WM_SETTINGCHANGE with "ConvertibleSlateMode" in the LPARAM. Note that this system metric doesn't apply to desktop PCs. In that case, use GetAutoRotationState.
  (:SM_CXBORDER 5)			;The width of a window border, in pixels. This is equivalent to the SM_CXEDGE value for windows with the 3-D look.
  (:SM_CXCURSOR 13)			;The width of a cursor, in pixels. The system cannot create cursors of other sizes.
  (:SM_CXDLGFRAME 7)			;This value is the same as SM_CXFIXEDFRAME.
  (:SM_CXDOUBLECLK 36)			;The width of the rectangle around the location of a first click in a double-click sequence, in pixels. The second click must occur within the rectangle that is defined by SM_CXDOUBLECLK and SM_CYDOUBLECLK for the system to consider the two clicks a double-click. The two clicks must also occur within a specified time.
					;To set the width of the double-click rectangle, call SystemParametersInfo with SPI_SETDOUBLECLKWIDTH.
  (:SM_CXDRAG 68)			;The number of pixels on either side of a mouse-down point that the mouse pointer can move before a drag operation begins. This allows the user to click and release the mouse button easily without unintentionally starting a drag operation. If this value is negative, it is subtracted from the left of the mouse-down point and added to the right of it.
  (:SM_CXEDGE 45)			;The width of a 3-D border, in pixels. This metric is the 3-D counterpart of SM_CXBORDER.
  (:SM_CXFIXEDFRAME 7)			;The thickness of the frame around the perimeter of a window that has a caption but is not sizable, in pixels. SM_CXFIXEDFRAME is the height of the horizontal border, and SM_CYFIXEDFRAME is the width of the vertical border.
					;This value is the same as SM_CXDLGFRAME.
  (:SM_CXFOCUSBORDER 83)		;The width of the left and right edges of the focus rectangle that the DrawFocusRect draws. This value is in pixels.
					;Windows 2000:  This value is not supported.
  (:SM_CXFRAME 32)			;This value is the same as SM_CXSIZEFRAME.
  (:SM_CXFULLSCREEN 16)			;The width of the client area for a full-screen window on the primary display monitor, in pixels. To get the coordinates of the portion of the screen that is not obscured by the system taskbar or by application desktop toolbars, call the SystemParametersInfo function with the SPI_GETWORKAREA value.
  (:SM_CXHSCROLL 21)			;The width of the arrow bitmap on a horizontal scroll bar, in pixels.
  (:SM_CXHTHUMB 10)			;The width of the thumb box in a horizontal scroll bar, in pixels.
  (:SM_CXICON 11)			;The default width of an icon, in pixels. The LoadIcon function can load only icons with the dimensions that SM_CXICON and SM_CYICON specifies.
  (:SM_CXICONSPACING 38)		;The width of a grid cell for items in large icon view, in pixels. Each item fits into a rectangle of size SM_CXICONSPACING by SM_CYICONSPACING when arranged. This value is always greater than or equal to SM_CXICON.
  (:SM_CXMAXIMIZED 61)			;The default width, in pixels, of a maximized top-level window on the primary display monitor.
  (:SM_CXMAXTRACK 59)			;The default maximum width of a window that has a caption and sizing borders, in pixels. This metric refers to the entire desktop. The user cannot drag the window frame to a size larger than these dimensions. A window can override this value by processing the WM_GETMINMAXINFO message.
  (:SM_CXMENUCHECK 71)			;The width of the default menu check-mark bitmap, in pixels.
  (:SM_CXMENUSIZE 54)			;The width of menu bar buttons, such as the child window close button that is used in the multiple document interface, in pixels.
  (:SM_CXMIN 28)			;The minimum width of a window, in pixels.
  (:SM_CXMINIMIZED 57)			;The width of a minimized window, in pixels.
  (:SM_CXMINSPACING 47)			;The width of a grid cell for a minimized window, in pixels. Each minimized window fits into a rectangle this size when arranged. This value is always greater than or equal to SM_CXMINIMIZED.
  (:SM_CXMINTRACK 34)			;The minimum tracking width of a window, in pixels. The user cannot drag the window frame to a size smaller than these dimensions. A window can override this value by processing the WM_GETMINMAXINFO message.
  (:SM_CXPADDEDBORDER 92)		;The amount of border padding for captioned windows, in pixels.
					;Windows XP/2000:  This value is not supported.
  (:SM_CXSCREEN 0)			;The width of the screen of the primary display monitor, in pixels. This is the same value obtained by calling GetDeviceCaps as follows: GetDeviceCaps( hdcPrimaryMonitor, HORZRES).
  (:SM_CXSIZE 30)			;The width of a button in a window caption or title bar, in pixels.
  (:SM_CXSIZEFRAME 32)			;The thickness of the sizing border around the perimeter of a window that can be resized, in pixels. SM_CXSIZEFRAME is the width of the horizontal border, and SM_CYSIZEFRAME is the height of the vertical border.
					;This value is the same as SM_CXFRAME.
  (:SM_CXSMICON 49)			;The recommended width of a small icon, in pixels. Small icons typically appear in window captions and in small icon view.
  (:SM_CXSMSIZE 52)			;The width of small caption buttons, in pixels.
  (:SM_CXVIRTUALSCREEN 78)		;The width of the virtual screen, in pixels. The virtual screen is the bounding rectangle of all display monitors. The SM_XVIRTUALSCREEN metric is the coordinates for the left side of the virtual screen.
  (:SM_CXVSCROLL 2)			;The width of a vertical scroll bar, in pixels.
  (:SM_CYBORDER 6)			;The height of a window border, in pixels. This is equivalent to the SM_CYEDGE value for windows with the 3-D look.
  (:SM_CYCAPTION 4)			;The height of a caption area, in pixels.
  (:SM_CYCURSOR 14)			;The height of a cursor, in pixels. The system cannot create cursors of other sizes.
  (:SM_CYDLGFRAME 8)			;This value is the same as SM_CYFIXEDFRAME.
  (:SM_CYDOUBLECLK 37)			;The height of the rectangle around the location of a first click in a double-click sequence, in pixels. The second click must occur within the rectangle defined by SM_CXDOUBLECLK and SM_CYDOUBLECLK for the system to consider the two clicks a double-click. The two clicks must also occur within a specified time.
					;To set the height of the double-click rectangle, call SystemParametersInfo with SPI_SETDOUBLECLKHEIGHT.
  (:SM_CYDRAG 69)			;The number of pixels above and below a mouse-down point that the mouse pointer can move before a drag operation begins. This allows the user to click and release the mouse button easily without unintentionally starting a drag operation. If this value is negative, it is subtracted from above the mouse-down point and added below it.
  (:SM_CYEDGE 46)			;The height of a 3-D border, in pixels. This is the 3-D counterpart of SM_CYBORDER.
  (:SM_CYFIXEDFRAME 8)			;The thickness of the frame around the perimeter of a window that has a caption but is not sizable, in pixels. SM_CXFIXEDFRAME is the height of the horizontal border, and SM_CYFIXEDFRAME is the width of the vertical border.
					;This value is the same as SM_CYDLGFRAME.
  (:SM_CYFOCUSBORDER 84)		;The height of the top and bottom edges of the focus rectangle drawn by DrawFocusRect. This value is in pixels.
					;Windows 2000:  This value is not supported.
  (:SM_CYFRAME 33)			;This value is the same as SM_CYSIZEFRAME.
  (:SM_CYFULLSCREEN 17)			;The height of the client area for a full-screen window on the primary display monitor, in pixels. To get the coordinates of the portion of the screen not obscured by the system taskbar or by application desktop toolbars, call the SystemParametersInfo function with the SPI_GETWORKAREA value.
  (:SM_CYHSCROLL 3)			;The height of a horizontal scroll bar, in pixels.
  (:SM_CYICON 12)			;The default height of an icon, in pixels. The LoadIcon function can load only icons with the dimensions SM_CXICON and SM_CYICON.
  (:SM_CYICONSPACING 39)		;The height of a grid cell for items in large icon view, in pixels. Each item fits into a rectangle of size SM_CXICONSPACING by SM_CYICONSPACING when arranged. This value is always greater than or equal to SM_CYICON.
  (:SM_CYKANJIWINDOW 18)		;For double byte character set versions of the system, this is the height of the Kanji window at the bottom of the screen, in pixels.
  (:SM_CYMAXIMIZED 62)			;The default height, in pixels, of a maximized top-level window on the primary display monitor.
  (:SM_CYMAXTRACK 60)			;The default maximum height of a window that has a caption and sizing borders, in pixels. This metric refers to the entire desktop. The user cannot drag the window frame to a size larger than these dimensions. A window can override this value by processing the WM_GETMINMAXINFO message.
  (:SM_CYMENU 15)			;The height of a single-line menu bar, in pixels.
  (:SM_CYMENUCHECK 72)			;The height of the default menu check-mark bitmap, in pixels.
  (:SM_CYMENUSIZE 55)			;The height of menu bar buttons, such as the child window close button that is used in the multiple document interface, in pixels.
  (:SM_CYMIN 29)			;The minimum height of a window, in pixels.
  (:SM_CYMINIMIZED 58)			;The height of a minimized window, in pixels.
  (:SM_CYMINSPACING 48)			;The height of a grid cell for a minimized window, in pixels. Each minimized window fits into a rectangle this size when arranged. This value is always greater than or equal to SM_CYMINIMIZED.
  (:SM_CYMINTRACK 35)			;The minimum tracking height of a window, in pixels. The user cannot drag the window frame to a size smaller than these dimensions. A window can override this value by processing the WM_GETMINMAXINFO message.
  (:SM_CYSCREEN 1)			;The height of the screen of the primary display monitor, in pixels. This is the same value obtained by calling GetDeviceCaps as follows: GetDeviceCaps( hdcPrimaryMonitor, VERTRES).
  (:SM_CYSIZE 31)			;The height of a button in a window caption or title bar, in pixels.
  (:SM_CYSIZEFRAME 33)			;The thickness of the sizing border around the perimeter of a window that can be resized, in pixels. SM_CXSIZEFRAME is the width of the horizontal border, and SM_CYSIZEFRAME is the height of the vertical border.
					;This value is the same as SM_CYFRAME.
  (:SM_CYSMCAPTION 51)			;The height of a small caption, in pixels.
  (:SM_CYSMICON 50)			;The recommended height of a small icon, in pixels. Small icons typically appear in window captions and in small icon view.
  (:SM_CYSMSIZE 53)			;The height of small caption buttons, in pixels.
  (:SM_CYVIRTUALSCREEN 79)		;The height of the virtual screen, in pixels. The virtual screen is the bounding rectangle of all display monitors. The SM_YVIRTUALSCREEN metric is the coordinates for the top of the virtual screen.
  (:SM_CYVSCROLL 20)			;The height of the arrow bitmap on a vertical scroll bar, in pixels.
  (:SM_CYVTHUMB 9)			;The height of the thumb box in a vertical scroll bar, in pixels.
  (:SM_DBCSENABLED 42)			;Nonzero if User32.dll supports DBCS; otherwise, 0.
  (:SM_DEBUG 22)			;Nonzero if the debug version of User.exe is installed; otherwise, 0.
  (:SM_DIGITIZER 94)			;Nonzero if the current operating system is Windows 7 or Windows Server 2008 R2 and the Tablet PC Input service is started; otherwise, 0. The return value is a bitmask that specifies the type of digitizer input supported by the device. For more information, see Remarks.
					;Windows Server 2008, Windows Vista, and Windows XP/2000:  This value is not supported.
  (:SM_IMMENABLED 82)			;Nonzero if Input Method Manager/Input Method Editor features are enabled; otherwise, 0.
					;SM_IMMENABLED indicates whether the system is ready to use a Unicode-based IME on a Unicode application. To ensure that a language-dependent IME works, check SM_DBCSENABLED and the system ANSI code page. Otherwise the ANSI-to-Unicode conversion may not be performed correctly, or some components like fonts or registry settings may not be present.
  (:SM_MAXIMUMTOUCHES 95)		;Nonzero if there are digitizers in the system; otherwise, 0.
					;SM_MAXIMUMTOUCHES returns the aggregate maximum of the maximum number of contacts supported by every digitizer in the system. If the system has only single-touch digitizers, the return value is 1. If the system has multi-touch digitizers, the return value is the number of simultaneous contacts the hardware can provide.
					;Windows Server 2008, Windows Vista, and Windows XP/2000:  This value is not supported.
  (:SM_MEDIACENTER 87)			;Nonzero if the current operating system is the Windows XP, Media Center Edition, 0 if not.
  (:SM_MENUDROPALIGNMENT 40)		;Nonzero if drop-down menus are right-aligned with the corresponding menu-bar item; 0 if the menus are left-aligned.
  (:SM_MIDEASTENABLED 74)		;Nonzero if the system is enabled for Hebrew and Arabic languages, 0 if not.
  (:SM_MOUSEPRESENT 19)			;Nonzero if a mouse is installed; otherwise, 0. This value is rarely zero, because of support for virtual mice and because some systems detect the presence of the port instead of the presence of a mouse.
  (:SM_MOUSEHORIZONTALWHEELPRESENT 91)	;Nonzero if a mouse with a horizontal scroll wheel is installed; otherwise 0.
  (:SM_MOUSEWHEELPRESENT 75)		;Nonzero if a mouse with a vertical scroll wheel is installed; otherwise 0.
  (:SM_NETWORK 63)			;The least significant bit is set if a network is present; otherwise, it is cleared. The other bits are reserved for future use.
  (:SM_PENWINDOWS 41)			;Nonzero if the Microsoft Windows for Pen computing extensions are installed; zero otherwise.
  (:SM_REMOTECONTROL #x2001)		;This system metric is used in a Terminal Services environment to determine if the current Terminal Server session is being remotely controlled. Its value is nonzero if the current session is remotely controlled; otherwise, 0.
					;You can use terminal services management tools such as Terminal Services Manager (tsadmin.msc) and shadow.exe to control a remote session. When a session is being remotely controlled, another user can view the contents of that session and potentially interact with it.
  (:SM_REMOTESESSION #x1000)		;This system metric is used in a Terminal Services environment. If the calling process is associated with a Terminal Services client session, the return value is nonzero. If the calling process is associated with the Terminal Services console session, the return value is 0.
					;Windows Server 2003 and Windows XP:  The console session is not necessarily the physical console. For more information, see WTSGetActiveConsoleSessionId.
  (:SM_SAMEDISPLAYFORMAT 81)		;Nonzero if all the display monitors have the same color format, otherwise, 0. Two displays can have the same bit depth, but different color formats. For example, the red, green, and blue pixels can be encoded with different numbers of bits, or those bits can be located in different places in a pixel color value.
  (:SM_SECURE 44)			;This system metric should be ignored; it always returns 0.
  (:SM_SERVERR2 89)			;The build number if the system is Windows Server 2003 R2; otherwise, 0.
  (:SM_SHOWSOUNDS 70)			;Nonzero if the user requires an application to present information visually in situations where it would otherwise present the information only in audible form; otherwise, 0.
  (:SM_SHUTTINGDOWN #x2000)		;Nonzero if the current session is shutting down; otherwise, 0.
					;Windows 2000:  This value is not supported.
  (:SM_SLOWMACHINE 73)			;Nonzero if the computer has a low-end (slow) processor; otherwise, 0.
  (:SM_STARTER 88)			;Nonzero if the current operating system is Windows 7 Starter Edition, Windows Vista Starter, or Windows XP Starter Edition; otherwise, 0.
  (:SM_SWAPBUTTON 23)			;Nonzero if the meanings of the left and right mouse buttons are swapped; otherwise, 0.
  (:SM_SYSTEMDOCKED #x2004)		;Reflects the state of the docking mode, 0 for Undocked Mode and non-zero otherwise. When this system metric changes, the system sends a broadcast message via WM_SETTINGCHANGE with "SystemDockMode" in the LPARAM.
  (:SM_TABLETPC 86)			;Nonzero if the current operating system is the Windows XP Tablet PC edition or if the current operating system is Windows Vista or Windows 7 and the Tablet PC Input service is started; otherwise, 0. The SM_DIGITIZER setting indicates the type of digitizer input supported by a device running Windows 7 or Windows Server 2008 R2. For more information, see Remarks.
  (:SM_XVIRTUALSCREEN 76)		;The coordinates for the left side of the virtual screen. The virtual screen is the bounding rectangle of all display monitors. The SM_CXVIRTUALSCREEN metric is the width of the virtual screen.
  (:SM_YVIRTUALSCREEN 77)		;The coordinates for the top of the virtual screen. The virtual screen is the bounding rectangle of all display monitors. The SM_CYVIRTUALSCREEN metric is the height of the virtual screen.
  )

(defcenum (COMPUTER_NAME_FORMAT_ENUM :uint)
  :ComputerNameNetBIOS
  :ComputerNameDnsHostname
  :ComputerNameDnsDomain
  :ComputerNameDnsFullyQualified
  :ComputerNamePhysicalNetBIOS
  :ComputerNamePhysicalDnsHostname
  :ComputerNamePhysicalDnsDomain
  :ComputerNamePhysicalDnsFullyQualified
  :ComputerNameMax)


(defcenum (EXTENDED_NAME_FORMAT_ENUM :uint)
  (:NameUnknown           0)
  (:NameFullyQualifiedDN  1)
  (:NameSamCompatible     2)
  (:NameDisplay           3)
  (:NameUniqueId          6)
  (:NameCanonical         7)
  (:NameUserPrincipal     8)
  (:NameCanonicalEx       9)
  (:NameServicePrincipal  10)
  (:NameDnsDomain         12))

(defcenum (GWR_RESULT_ENUM :int)
  (:ERROR               0)
  (:NULLREGION          1)
  (:SIMPLEREGION        2)
  (:COMPLEXREGION       3))

(defmacro MAKELANGID (p s)
  `(logior (ash (foreign-enum-value 'SUBLANG_ENUM ,s) 10) (foreign-enum-value 'LANG_ENUM ,p)))

(defcenum (LANG_ENUM WORD)
  (:LANG_NEUTRAL                     #x00)
  (:LANG_INVARIANT                   #x7f)
  (:LANG_AFRIKAANS                   #x36)
  (:LANG_ALBANIAN                    #x1c)
  (:LANG_ALSATIAN                    #x84)
  (:LANG_AMHARIC                     #x5e)
  (:LANG_ARABIC                      #x01)
  (:LANG_ARMENIAN                    #x2b)
  (:LANG_ASSAMESE                    #x4d)
  (:LANG_AZERI                       #x2c)
  (:LANG_AZERBAIJANI                 #x2c)
  (:LANG_BANGLA                      #x45)
  (:LANG_BASHKIR                     #x6d)
  (:LANG_BASQUE                      #x2d)
  (:LANG_BELARUSIAN                  #x23)
  (:LANG_BENGALI                     #x45)
  (:LANG_BRETON                      #x7e)
  (:LANG_BOSNIAN                     #x1a)
  (:LANG_BOSNIAN_NEUTRAL           #x781a)
  (:LANG_BULGARIAN                   #x02)
  (:LANG_CATALAN                     #x03)
  (:LANG_CENTRAL_KURDISH             #x92)
  (:LANG_CHEROKEE                    #x5c)
  (:LANG_CHINESE                     #x04)
  (:LANG_CHINESE_SIMPLIFIED          #x04)
  (:LANG_CHINESE_TRADITIONAL       #x7c04)
  (:LANG_CORSICAN                    #x83)
  (:LANG_CROATIAN                    #x1a)
  (:LANG_CZECH                       #x05)
  (:LANG_DANISH                      #x06)
  (:LANG_DARI                        #x8c)
  (:LANG_DIVEHI                      #x65)
  (:LANG_DUTCH                       #x13)
  (:LANG_ENGLISH                     #x09)
  (:LANG_ESTONIAN                    #x25)
  (:LANG_FAEROESE                    #x38)
  (:LANG_FARSI                       #x29)
  (:LANG_FILIPINO                    #x64)
  (:LANG_FINNISH                     #x0b)
  (:LANG_FRENCH                      #x0c)
  (:LANG_FRISIAN                     #x62)
  (:LANG_FULAH                       #x67)
  (:LANG_GALICIAN                    #x56)
  (:LANG_GEORGIAN                    #x37)
  (:LANG_GERMAN                      #x07)
  (:LANG_GREEK                       #x08)
  (:LANG_GREENLANDIC                 #x6f)
  (:LANG_GUJARATI                    #x47)
  (:LANG_HAUSA                       #x68)
  (:LANG_HAWAIIAN                    #x75)
  (:LANG_HEBREW                      #x0d)
  (:LANG_HINDI                       #x39)
  (:LANG_HUNGARIAN                   #x0e)
  (:LANG_ICELANDIC                   #x0f)
  (:LANG_IGBO                        #x70)
  (:LANG_INDONESIAN                  #x21)
  (:LANG_INUKTITUT                   #x5d)
  (:LANG_IRISH                       #x3c)
  (:LANG_ITALIAN                     #x10)
  (:LANG_JAPANESE                    #x11)
  (:LANG_KANNADA                     #x4b)
  (:LANG_KASHMIRI                    #x60)
  (:LANG_KAZAK                       #x3f)
  (:LANG_KHMER                       #x53)
  (:LANG_KICHE                       #x86)
  (:LANG_KINYARWANDA                 #x87)
  (:LANG_KONKANI                     #x57)
  (:LANG_KOREAN                      #x12)
  (:LANG_KYRGYZ                      #x40)
  (:LANG_LAO                         #x54)
  (:LANG_LATVIAN                     #x26)
  (:LANG_LITHUANIAN                  #x27)
  (:LANG_LOWER_SORBIAN               #x2e)
  (:LANG_LUXEMBOURGISH               #x6e)
  (:LANG_MACEDONIAN                  #x2f)
  (:LANG_MALAY                       #x3e)
  (:LANG_MALAYALAM                   #x4c)
  (:LANG_MALTESE                     #x3a)
  (:LANG_MANIPURI                    #x58)
  (:LANG_MAORI                       #x81)
  (:LANG_MAPUDUNGUN                  #x7a)
  (:LANG_MARATHI                     #x4e)
  (:LANG_MOHAWK                      #x7c)
  (:LANG_MONGOLIAN                   #x50)
  (:LANG_NEPALI                      #x61)
  (:LANG_NORWEGIAN                   #x14)
  (:LANG_OCCITAN                     #x82)
  (:LANG_ODIA                        #x48)
  (:LANG_ORIYA                       #x48)
  (:LANG_PASHTO                      #x63)
  (:LANG_PERSIAN                     #x29)
  (:LANG_POLISH                      #x15)
  (:LANG_PORTUGUESE                  #x16)
  (:LANG_PULAR                       #x67)
  (:LANG_PUNJABI                     #x46)
  (:LANG_QUECHUA                     #x6b)
  (:LANG_ROMANIAN                    #x18)
  (:LANG_ROMANSH                     #x17)
  (:LANG_RUSSIAN                     #x19)
  (:LANG_SAKHA                       #x85)
  (:LANG_SAMI                        #x3b)
  (:LANG_SANSKRIT                    #x4f)
  (:LANG_SCOTTISH_GAELIC             #x91)
  (:LANG_SERBIAN                     #x1a)
  (:LANG_SERBIAN_NEUTRAL           #x7c1a)
  (:LANG_SINDHI                      #x59)
  (:LANG_SINHALESE                   #x5b)
  (:LANG_SLOVAK                      #x1b)
  (:LANG_SLOVENIAN                   #x24)
  (:LANG_SOTHO                       #x6c)
  (:LANG_SPANISH                     #x0a)
  (:LANG_SWAHILI                     #x41)
  (:LANG_SWEDISH                     #x1d)
  (:LANG_SYRIAC                      #x5a)
  (:LANG_TAJIK                       #x28)
  (:LANG_TAMAZIGHT                   #x5f)
  (:LANG_TAMIL                       #x49)
  (:LANG_TATAR                       #x44)
  (:LANG_TELUGU                      #x4a)
  (:LANG_THAI                        #x1e)
  (:LANG_TIBETAN                     #x51)
  (:LANG_TIGRIGNA                    #x73)
  (:LANG_TIGRINYA                    #x73)
  (:LANG_TSWANA                      #x32)
  (:LANG_TURKISH                     #x1f)
  (:LANG_TURKMEN                     #x42)
  (:LANG_UIGHUR                      #x80)
  (:LANG_UKRAINIAN                   #x22)
  (:LANG_UPPER_SORBIAN               #x2e)
  (:LANG_URDU                        #x20)
  (:LANG_UZBEK                       #x43)
  (:LANG_VALENCIAN                   #x03)
  (:LANG_VIETNAMESE                  #x2a)
  (:LANG_WELSH                       #x52)
  (:LANG_WOLOF                       #x88)
  (:LANG_XHOSA                       #x34)
  (:LANG_YAKUT                       #x85)
  (:LANG_YI                          #x78)
  (:LANG_YORUBA                      #x6a)
  (:LANG_ZULU                        #x35))

(defcenum (SUBLANG_ENUM WORD)
  (:SUBLANG_NEUTRAL                             #x00)
  (:SUBLANG_DEFAULT                             #x01)
  (:SUBLANG_SYS_DEFAULT                         #x02)
  (:SUBLANG_CUSTOM_DEFAULT                      #x03)
  (:SUBLANG_CUSTOM_UNSPECIFIED                  #x04)
  (:SUBLANG_UI_CUSTOM_DEFAULT                   #x05)
  (:SUBLANG_AFRIKAANS_SOUTH_AFRICA              #x01)
  (:SUBLANG_ALBANIAN_ALBANIA                    #x01)
  (:SUBLANG_ALSATIAN_FRANCE                     #x01)
  (:SUBLANG_AMHARIC_ETHIOPIA                    #x01)
  (:SUBLANG_ARABIC_SAUDI_ARABIA                 #x01)
  (:SUBLANG_ARABIC_IRAQ                         #x02)
  (:SUBLANG_ARABIC_EGYPT                        #x03)
  (:SUBLANG_ARABIC_LIBYA                        #x04)
  (:SUBLANG_ARABIC_ALGERIA                      #x05)
  (:SUBLANG_ARABIC_MOROCCO                      #x06)
  (:SUBLANG_ARABIC_TUNISIA                      #x07)
  (:SUBLANG_ARABIC_OMAN                         #x08)
  (:SUBLANG_ARABIC_YEMEN                        #x09)
  (:SUBLANG_ARABIC_SYRIA                        #x0a)
  (:SUBLANG_ARABIC_JORDAN                       #x0b)
  (:SUBLANG_ARABIC_LEBANON                      #x0c)
  (:SUBLANG_ARABIC_KUWAIT                       #x0d)
  (:SUBLANG_ARABIC_UAE                          #x0e)
  (:SUBLANG_ARABIC_BAHRAIN                      #x0f)
  (:SUBLANG_ARABIC_QATAR                        #x10)
  (:SUBLANG_ARMENIAN_ARMENIA                    #x01)
  (:SUBLANG_ASSAMESE_INDIA                      #x01)
  (:SUBLANG_AZERI_LATIN                         #x01)
  (:SUBLANG_AZERI_CYRILLIC                      #x02)
  (:SUBLANG_AZERBAIJANI_AZERBAIJAN_LATIN        #x01)
  (:SUBLANG_AZERBAIJANI_AZERBAIJAN_CYRILLIC     #x02)
  (:SUBLANG_BANGLA_INDIA                        #x01)
  (:SUBLANG_BANGLA_BANGLADESH                   #x02)
  (:SUBLANG_BASHKIR_RUSSIA                      #x01)
  (:SUBLANG_BASQUE_BASQUE                       #x01)
  (:SUBLANG_BELARUSIAN_BELARUS                  #x01)
  (:SUBLANG_BENGALI_INDIA                       #x01)
  (:SUBLANG_BENGALI_BANGLADESH                  #x02)
  (:SUBLANG_BOSNIAN_BOSNIA_HERZEGOVINA_LATIN    #x05)
  (:SUBLANG_BOSNIAN_BOSNIA_HERZEGOVINA_CYRILLIC #x08)
  (:SUBLANG_BRETON_FRANCE                       #x01)
  (:SUBLANG_BULGARIAN_BULGARIA                  #x01)
  (:SUBLANG_CATALAN_CATALAN                     #x01)
  (:SUBLANG_CENTRAL_KURDISH_IRAQ                #x01)
  (:SUBLANG_CHEROKEE_CHEROKEE                   #x01)
  (:SUBLANG_CHINESE_TRADITIONAL                 #x01)
  (:SUBLANG_CHINESE_SIMPLIFIED                  #x02)
  (:SUBLANG_CHINESE_HONGKONG                    #x03)
  (:SUBLANG_CHINESE_SINGAPORE                   #x04)
  (:SUBLANG_CHINESE_MACAU                       #x05)
  (:SUBLANG_CORSICAN_FRANCE                     #x01)
  (:SUBLANG_CZECH_CZECH_REPUBLIC                #x01)
  (:SUBLANG_CROATIAN_CROATIA                    #x01)
  (:SUBLANG_CROATIAN_BOSNIA_HERZEGOVINA_LATIN   #x04)
  (:SUBLANG_DANISH_DENMARK                      #x01)
  (:SUBLANG_DARI_AFGHANISTAN                    #x01)
  (:SUBLANG_DIVEHI_MALDIVES                     #x01)
  (:SUBLANG_DUTCH                               #x01)
  (:SUBLANG_DUTCH_BELGIAN                       #x02)
  (:SUBLANG_ENGLISH_US                          #x01)
  (:SUBLANG_ENGLISH_UK                          #x02)
  (:SUBLANG_ENGLISH_AUS                         #x03)
  (:SUBLANG_ENGLISH_CAN                         #x04)
  (:SUBLANG_ENGLISH_NZ                          #x05)
  (:SUBLANG_ENGLISH_EIRE                        #x06)
  (:SUBLANG_ENGLISH_SOUTH_AFRICA                #x07)
  (:SUBLANG_ENGLISH_JAMAICA                     #x08)
  (:SUBLANG_ENGLISH_CARIBBEAN                   #x09)
  (:SUBLANG_ENGLISH_BELIZE                      #x0a)
  (:SUBLANG_ENGLISH_TRINIDAD                    #x0b)
  (:SUBLANG_ENGLISH_ZIMBABWE                    #x0c)
  (:SUBLANG_ENGLISH_PHILIPPINES                 #x0d)
  (:SUBLANG_ENGLISH_INDIA                       #x10)
  (:SUBLANG_ENGLISH_MALAYSIA                    #x11)
  (:SUBLANG_ENGLISH_SINGAPORE                   #x12)
  (:SUBLANG_ESTONIAN_ESTONIA                    #x01)
  (:SUBLANG_FAEROESE_FAROE_ISLANDS              #x01)
  (:SUBLANG_FILIPINO_PHILIPPINES                #x01)
  (:SUBLANG_FINNISH_FINLAND                     #x01)
  (:SUBLANG_FRENCH                              #x01)
  (:SUBLANG_FRENCH_BELGIAN                      #x02)
  (:SUBLANG_FRENCH_CANADIAN                     #x03)
  (:SUBLANG_FRENCH_SWISS                        #x04)
  (:SUBLANG_FRENCH_LUXEMBOURG                   #x05)
  (:SUBLANG_FRENCH_MONACO                       #x06)
  (:SUBLANG_FRISIAN_NETHERLANDS                 #x01)
  (:SUBLANG_FULAH_SENEGAL                       #x02)
  (:SUBLANG_GALICIAN_GALICIAN                   #x01)
  (:SUBLANG_GEORGIAN_GEORGIA                    #x01)
  (:SUBLANG_GERMAN                              #x01)
  (:SUBLANG_GERMAN_SWISS                        #x02)
  (:SUBLANG_GERMAN_AUSTRIAN                     #x03)
  (:SUBLANG_GERMAN_LUXEMBOURG                   #x04)
  (:SUBLANG_GERMAN_LIECHTENSTEIN                #x05)
  (:SUBLANG_GREEK_GREECE                        #x01)
  (:SUBLANG_GREENLANDIC_GREENLAND               #x01)
  (:SUBLANG_GUJARATI_INDIA                      #x01)
  (:SUBLANG_HAUSA_NIGERIA_LATIN                 #x01)
  (:SUBLANG_HAWAIIAN_US                         #x01)
  (:SUBLANG_HEBREW_ISRAEL                       #x01)
  (:SUBLANG_HINDI_INDIA                         #x01)
  (:SUBLANG_HUNGARIAN_HUNGARY                   #x01)
  (:SUBLANG_ICELANDIC_ICELAND                   #x01)
  (:SUBLANG_IGBO_NIGERIA                        #x01)
  (:SUBLANG_INDONESIAN_INDONESIA                #x01)
  (:SUBLANG_INUKTITUT_CANADA                    #x01)
  (:SUBLANG_INUKTITUT_CANADA_LATIN              #x02)
  (:SUBLANG_IRISH_IRELAND                       #x02)
  (:SUBLANG_ITALIAN                             #x01)
  (:SUBLANG_ITALIAN_SWISS                       #x02)
  (:SUBLANG_JAPANESE_JAPAN                      #x01)
  (:SUBLANG_KANNADA_INDIA                       #x01)
  (:SUBLANG_KASHMIRI_SASIA                      #x02)
  (:SUBLANG_KASHMIRI_INDIA                      #x02)
  (:SUBLANG_KAZAK_KAZAKHSTAN                    #x01)
  (:SUBLANG_KHMER_CAMBODIA                      #x01)
  (:SUBLANG_KICHE_GUATEMALA                     #x01)
  (:SUBLANG_KINYARWANDA_RWANDA                  #x01)
  (:SUBLANG_KONKANI_INDIA                       #x01)
  (:SUBLANG_KOREAN                              #x01)
  (:SUBLANG_KYRGYZ_KYRGYZSTAN                   #x01)
  (:SUBLANG_LAO_LAO                             #x01)
  (:SUBLANG_LATVIAN_LATVIA                      #x01)
  (:SUBLANG_LITHUANIAN                          #x01)
  (:SUBLANG_LOWER_SORBIAN_GERMANY               #x02)
  (:SUBLANG_LUXEMBOURGISH_LUXEMBOURG            #x01)
  (:SUBLANG_MACEDONIAN_MACEDONIA                #x01)
  (:SUBLANG_MALAY_MALAYSIA                      #x01)
  (:SUBLANG_MALAY_BRUNEI_DARUSSALAM             #x02)
  (:SUBLANG_MALAYALAM_INDIA                     #x01)
  (:SUBLANG_MALTESE_MALTA                       #x01)
  (:SUBLANG_MAORI_NEW_ZEALAND                   #x01)
  (:SUBLANG_MAPUDUNGUN_CHILE                    #x01)
  (:SUBLANG_MARATHI_INDIA                       #x01)
  (:SUBLANG_MOHAWK_MOHAWK                       #x01)
  (:SUBLANG_MONGOLIAN_CYRILLIC_MONGOLIA         #x01)
  (:SUBLANG_MONGOLIAN_PRC                       #x02)
  (:SUBLANG_NEPALI_INDIA                        #x02)
  (:SUBLANG_NEPALI_NEPAL                        #x01)
  (:SUBLANG_NORWEGIAN_BOKMAL                    #x01)
  (:SUBLANG_NORWEGIAN_NYNORSK                   #x02)
  (:SUBLANG_OCCITAN_FRANCE                      #x01)
  (:SUBLANG_ODIA_INDIA                          #x01)
  (:SUBLANG_ORIYA_INDIA                         #x01)
  (:SUBLANG_PASHTO_AFGHANISTAN                  #x01)
  (:SUBLANG_PERSIAN_IRAN                        #x01)
  (:SUBLANG_POLISH_POLAND                       #x01)
  (:SUBLANG_PORTUGUESE                          #x02)
  (:SUBLANG_PORTUGUESE_BRAZILIAN                #x01)
  (:SUBLANG_PULAR_SENEGAL                       #x02)
  (:SUBLANG_PUNJABI_INDIA                       #x01)
  (:SUBLANG_PUNJABI_PAKISTAN                    #x02)
  (:SUBLANG_QUECHUA_BOLIVIA                     #x01)
  (:SUBLANG_QUECHUA_ECUADOR                     #x02)
  (:SUBLANG_QUECHUA_PERU                        #x03)
  (:SUBLANG_ROMANIAN_ROMANIA                    #x01)
  (:SUBLANG_ROMANSH_SWITZERLAND                 #x01)
  (:SUBLANG_RUSSIAN_RUSSIA                      #x01)
  (:SUBLANG_SAKHA_RUSSIA                        #x01)
  (:SUBLANG_SAMI_NORTHERN_NORWAY                #x01)
  (:SUBLANG_SAMI_NORTHERN_SWEDEN                #x02)
  (:SUBLANG_SAMI_NORTHERN_FINLAND               #x03)
  (:SUBLANG_SAMI_LULE_NORWAY                    #x04)
  (:SUBLANG_SAMI_LULE_SWEDEN                    #x05)
  (:SUBLANG_SAMI_SOUTHERN_NORWAY                #x06)
  (:SUBLANG_SAMI_SOUTHERN_SWEDEN                #x07)
  (:SUBLANG_SAMI_SKOLT_FINLAND                  #x08)
  (:SUBLANG_SAMI_INARI_FINLAND                  #x09)
  (:SUBLANG_SANSKRIT_INDIA                      #x01)
  (:SUBLANG_SCOTTISH_GAELIC                     #x01)
  (:SUBLANG_SERBIAN_BOSNIA_HERZEGOVINA_LATIN    #x06)
  (:SUBLANG_SERBIAN_BOSNIA_HERZEGOVINA_CYRILLIC #x07)
  (:SUBLANG_SERBIAN_MONTENEGRO_LATIN            #x0b)
  (:SUBLANG_SERBIAN_MONTENEGRO_CYRILLIC         #x0c)
  (:SUBLANG_SERBIAN_SERBIA_LATIN                #x09)
  (:SUBLANG_SERBIAN_SERBIA_CYRILLIC             #x0a)
  (:SUBLANG_SERBIAN_CROATIA                     #x01)
  (:SUBLANG_SERBIAN_LATIN                       #x02)
  (:SUBLANG_SERBIAN_CYRILLIC                    #x03)
  (:SUBLANG_SINDHI_INDIA                        #x01)
  (:SUBLANG_SINDHI_PAKISTAN                     #x02)
  (:SUBLANG_SINDHI_AFGHANISTAN                  #x02)
  (:SUBLANG_SINHALESE_SRI_LANKA                 #x01)
  (:SUBLANG_SOTHO_NORTHERN_SOUTH_AFRICA         #x01)
  (:SUBLANG_SLOVAK_SLOVAKIA                     #x01)
  (:SUBLANG_SLOVENIAN_SLOVENIA                  #x01)
  (:SUBLANG_SPANISH                             #x01)
  (:SUBLANG_SPANISH_MEXICAN                     #x02)
  (:SUBLANG_SPANISH_MODERN                      #x03)
  (:SUBLANG_SPANISH_GUATEMALA                   #x04)
  (:SUBLANG_SPANISH_COSTA_RICA                  #x05)
  (:SUBLANG_SPANISH_PANAMA                      #x06)
  (:SUBLANG_SPANISH_DOMINICAN_REPUBLIC          #x07)
  (:SUBLANG_SPANISH_VENEZUELA                   #x08)
  (:SUBLANG_SPANISH_COLOMBIA                    #x09)
  (:SUBLANG_SPANISH_PERU                        #x0a)
  (:SUBLANG_SPANISH_ARGENTINA                   #x0b)
  (:SUBLANG_SPANISH_ECUADOR                     #x0c)
  (:SUBLANG_SPANISH_CHILE                       #x0d)
  (:SUBLANG_SPANISH_URUGUAY                     #x0e)
  (:SUBLANG_SPANISH_PARAGUAY                    #x0f)
  (:SUBLANG_SPANISH_BOLIVIA                     #x10)
  (:SUBLANG_SPANISH_EL_SALVADOR                 #x11)
  (:SUBLANG_SPANISH_HONDURAS                    #x12)
  (:SUBLANG_SPANISH_NICARAGUA                   #x13)
  (:SUBLANG_SPANISH_PUERTO_RICO                 #x14)
  (:SUBLANG_SPANISH_US                          #x15)
  (:SUBLANG_SWAHILI_KENYA                       #x01)
  (:SUBLANG_SWEDISH                             #x01)
  (:SUBLANG_SWEDISH_FINLAND                     #x02)
  (:SUBLANG_SYRIAC_SYRIA                        #x01)
  (:SUBLANG_TAJIK_TAJIKISTAN                    #x01)
  (:SUBLANG_TAMAZIGHT_ALGERIA_LATIN             #x02)
  (:SUBLANG_TAMAZIGHT_MOROCCO_TIFINAGH          #x04)
  (:SUBLANG_TAMIL_INDIA                         #x01)
  (:SUBLANG_TAMIL_SRI_LANKA                     #x02)
  (:SUBLANG_TATAR_RUSSIA                        #x01)
  (:SUBLANG_TELUGU_INDIA                        #x01)
  (:SUBLANG_THAI_THAILAND                       #x01)
  (:SUBLANG_TIBETAN_PRC                         #x01)
  (:SUBLANG_TIGRIGNA_ERITREA                    #x02)
  (:SUBLANG_TIGRINYA_ERITREA                    #x02)
  (:SUBLANG_TIGRINYA_ETHIOPIA                   #x01)
  (:SUBLANG_TSWANA_BOTSWANA                     #x02)
  (:SUBLANG_TSWANA_SOUTH_AFRICA                 #x01)
  (:SUBLANG_TURKISH_TURKEY                      #x01)
  (:SUBLANG_TURKMEN_TURKMENISTAN                #x01)
  (:SUBLANG_UIGHUR_PRC                          #x01)
  (:SUBLANG_UKRAINIAN_UKRAINE                   #x01)
  (:SUBLANG_UPPER_SORBIAN_GERMANY               #x01)
  (:SUBLANG_URDU_PAKISTAN                       #x01)
  (:SUBLANG_URDU_INDIA                          #x02)
  (:SUBLANG_UZBEK_LATIN                         #x01)
  (:SUBLANG_UZBEK_CYRILLIC                      #x02)
  (:SUBLANG_VALENCIAN_VALENCIA                  #x02)
  (:SUBLANG_VIETNAMESE_VIETNAM                  #x01)
  (:SUBLANG_WELSH_UNITED_KINGDOM                #x01)
  (:SUBLANG_WOLOF_SENEGAL                       #x01)
  (:SUBLANG_XHOSA_SOUTH_AFRICA                  #x01)
  (:SUBLANG_YAKUT_RUSSIA                        #x01)
  (:SUBLANG_YI_PRC                              #x01)
  (:SUBLANG_YORUBA_NIGERIA                      #x01)
  (:SUBLANG_ZULU_SOUTH_AFRICA                   #x01)
  )

(defcenum (PF_ENUM DWORD)
  (:PF_ARM_64BIT_LOADSTORE_ATOMIC 25)		;The 64-bit load/store atomic instructions are available.
  (:PF_ARM_DIVIDE_INSTRUCTION_AVAILABLE 24)	;The divide instructions are available.
  (:PF_ARM_EXTERNAL_CACHE_AVAILABLE 26)		;The external cache is available.
  (:PF_ARM_FMAC_INSTRUCTIONS_AVAILABLE 27)	;The floating-point multiply-accumulate instruction is available.
  (:PF_ARM_VFP_32_REGISTERS_AVAILABLE 18)	;The VFP/Neon: 32 x 64bit register bank is present. This flag has the same meaning as PF_ARM_VFP_EXTENDED_REGISTERS.
  (:PF_3DNOW_INSTRUCTIONS_AVAILABLE 7)		;The 3D-Now instruction set is available.
  (:PF_CHANNELS_ENABLED 16)			;The processor channels are enabled.
  (:PF_COMPARE_EXCHANGE_DOUBLE 2)		;The atomic compare and exchange operation (cmpxchg) is available.
  (:PF_COMPARE_EXCHANGE128 14)			;The atomic compare and exchange 128-bit operation (cmpxchg16b) is available.Windows Server 2003 and Windows XP/2000:  This feature is not supported.
  (:PF_COMPARE64_EXCHANGE128 15)		;The atomic compare 64 and exchange 128-bit operation (cmp8xchg16) is available.Windows Server 2003 and Windows XP/2000:  This feature is not supported.
  (:PF_FASTFAIL_AVAILABLE 23)			;_fastfail() is available.
  (:PF_FLOATING_POINT_EMULATED 1)		;Floating-point operations are emulated using a software emulator.This function returns a nonzero value if floating-point operations are emulated; otherwise, it returns zero.
  (:PF_FLOATING_POINT_PRECISION_ERRATA 0)	;On a Pentium, a floating-point precision error can occur in rare circumstances.
  (:PF_MMX_INSTRUCTIONS_AVAILABLE 3)		;The MMX instruction set is available.
  (:PF_NX_ENABLED 12)				;Data execution prevention is enabled.Windows XP/2000:  This feature is not supported until Windows XP with SP2 and Windows Server 2003 with SP1.
  (:PF_PAE_ENABLED 9)				;The processor is PAE-enabled. For more information, see Physical Address Extension.All x64 processors always return a nonzero value for this feature.
  (:PF_RDTSC_INSTRUCTION_AVAILABLE 8)		;The RDTSC instruction is available.
  (:PF_RDWRFSGSBASE_AVAILABLE 22)		;RDFSBASE, RDGSBASE, WRFSBASE, and WRGSBASE instructions are available.
  (:PF_SECOND_LEVEL_ADDRESS_TRANSLATION 20)	;Second Level Address Translation is supported by the hardware.
  (:PF_SSE3_INSTRUCTIONS_AVAILABLE 13)		;The SSE3 instruction set is available.Windows Server 2003 and Windows XP/2000:  This feature is not supported.
  (:PF_VIRT_FIRMWARE_ENABLED 21)		;Virtualization is enabled in the firmware.
  (:PF_XMMI_INSTRUCTIONS_AVAILABLE 6)		;The SSE instruction set is available.
  (:PF_XMMI64_INSTRUCTIONS_AVAILABLE 10)	;The SSE2 instruction set is available.Windows 2000:  This feature is not supported.
  (:PF_XSAVE_ENABLED 17)			;The processor implements the XSAVE and XRSTOR instructions.Windows Server 2008, Windows Vista, Windows Server 2003, and Windows XP/2000:  This feature is not supported until Windows 7 and Windows Server 2008 R2.)
  )

(defbitfield (MDITILE_FLAG :uint)
  (:MDITILE_SKIPDISABLED	#x0002)		;Prevents disabled MDI child windows from being cascaded.
  (:MDITILE_ZORDER		#x0004)		;Arranges the windows in Z order. If this value is not specified, the windows are arranged using the order specified in the lpKids array.
  (:MDITILE_HORIZONTAL		#x0001)		;Tiles windows horizontally.
  (:MDITILE_VERTICAL		#x0000))	;Tiles windows vertically.

(defbitfield (PM_FLAG :uint)
  (:PM_NOREMOVE #x0000) ;Messages are not removed from the queue after processing by PeekMessage.
  (:PM_REMOVE	#x0001) ;Messages are removed from the queue after processing by PeekMessage.
  (:PM_NOYIELD	#x0002) ;Prevents the system from releasing any thread that is waiting for the caller to go idle (see WaitForInputIdle).Combine this value with either PM_NOREMOVE or PM_REMOVE.
  )

(defcenum (COLOR_ENUM :int)
  (:COLOR_3DDKSHADOW 21)		;Dark shadow for three-dimensional display elements.
  (:COLOR_3DFACE 15)			;Face color for three-dimensional display elements and for dialog box backgrounds.
  (:COLOR_3DHIGHLIGHT 20)		;Highlight color for three-dimensional display elements (for edges facing the light source.)
  (:COLOR_3DHILIGHT 20)			;Highlight color for three-dimensional display elements (for edges facing the light source.)
  (:COLOR_3DLIGHT 22)			;Light color for three-dimensional display elements (for edges facing the light source.)
  (:COLOR_3DSHADOW 16)			;Shadow color for three-dimensional display elements (for edges facing away from the light source).
  (:COLOR_ACTIVEBORDER 10)		;Active window border.
  (:COLOR_ACTIVECAPTION 2)		;Active window title bar.Specifies the left side color in the color gradient of an active window's title bar if the gradient effect is enabled.
  (:COLOR_APPWORKSPACE 12)		;Background color of multiple document interface (MDI) applications.
  (:COLOR_BACKGROUND 1)			;Desktop.
  (:COLOR_BTNFACE 15)			;Face color for three-dimensional display elements and for dialog box backgrounds.
  (:COLOR_BTNHIGHLIGHT 20)		;Highlight color for three-dimensional display elements (for edges facing the light source.)
  (:COLOR_BTNHILIGHT 20)		;Highlight color for three-dimensional display elements (for edges facing the light source.)
  (:COLOR_BTNSHADOW 16)			;Shadow color for three-dimensional display elements (for edges facing away from the light source).
  (:COLOR_BTNTEXT 18)			;Text on push buttons.
  (:COLOR_CAPTIONTEXT 9)		;Text in caption, size box, and scroll bar arrow box.
  (:COLOR_DESKTOP 1)			;Desktop.
  (:COLOR_GRADIENTACTIVECAPTION 27)	;Right side color in the color gradient of an active window's title bar. COLOR_ACTIVECAPTION specifies the left side color. Use SPI_GETGRADIENTCAPTIONS with the SystemParametersInfo function to determine whether the gradient effect is enabled.
  (:COLOR_GRADIENTINACTIVECAPTION 28)	;Right side color in the color gradient of an inactive window's title bar. COLOR_INACTIVECAPTION specifies the left side color.
  (:COLOR_GRAYTEXT 17)			;Grayed (disabled) text. This color is set to 0 if the current display driver does not support a solid gray color.
  (:COLOR_HIGHLIGHT 13)			;Item(s) selected in a control.
  (:COLOR_HIGHLIGHTTEXT 14)		;Text of item(s) selected in a control.
  (:COLOR_HOTLIGHT 26)			;Color for a hyperlink or hot-tracked item.
  (:COLOR_INACTIVEBORDER 11)		;Inactive window border.
  (:COLOR_INACTIVECAPTION 3)		;Inactive window caption.Specifies the left side color in the color gradient of an inactive window's title bar if the gradient effect is enabled.
  (:COLOR_INACTIVECAPTIONTEXT 19)	;Color of text in an inactive caption.
  (:COLOR_INFOBK 24)			;Background color for tooltip controls.
  (:COLOR_INFOTEXT 23)			;Text color for tooltip controls.
  (:COLOR_MENU 4)			;Menu background.
  (:COLOR_MENUHILIGHT 29)		;The color used to highlight menu items when the menu appears as a flat menu (see SystemParametersInfo). The highlighted menu item is outlined with COLOR_HIGHLIGHT.Windows 2000:  This value is not supported.
  (:COLOR_MENUBAR 30)			;The background color for the menu bar when menus appear as flat menus (see SystemParametersInfo). However, COLOR_MENU continues to specify the background color of the menu popup.Windows 2000:  This value is not supported.
  (:COLOR_MENUTEXT 7)			;Text in menus.
  (:COLOR_SCROLLBAR 0)			;Scroll bar gray area.
  (:COLOR_WINDOW 5)			;Window background.
  (:COLOR_WINDOWFRAME 6)		;Window frame.
  (:COLOR_WINDOWTEXT 8)			;Text in windows.
  )

(defbitfield (MB_FLAG :uint)
  (:MB_ABORTRETRYIGNORE		#x00000002)	;The message box contains three push buttons: Abort, Retry, and Ignore.
  (:MB_CANCELTRYCONTINUE	#x00000006)	;The message box contains three push buttons: Cancel, Try Again, Continue. Use this message box type instead of MB_ABORTRETRYIGNORE.
  (:MB_HELP			#x00004000)	;Adds a Help button to the message box. When the user clicks the Help button or presses F1, the system sends a WM_HELP message to the owner.
  (:MB_OK			#x00000000)	;The message box contains one push button: OK. This is the default.
  (:MB_OKCANCEL			#x00000001)	;The message box contains two push buttons: OK and Cancel.
  (:MB_RETRYCANCEL		#x00000005)	;The message box contains two push buttons: Retry and Cancel.
  (:MB_YESNO			#x00000004)	;The message box contains two push buttons: Yes and No.
  (:MB_YESNOCANCEL		#x00000003)	;The message box contains three push buttons: Yes, No, and Cancel.
  (:MB_ICONEXCLAMATION		#x00000030)	;An exclamation-point icon appears in the message box.
  (:MB_ICONWARNING		#x00000030)	;An exclamation-point icon appears in the message box.
  (:MB_ICONINFORMATION		#x00000040)	;An icon consisting of a lowercase letter i in a circle appears in the message box.
  (:MB_ICONASTERISK		#x00000040)	;An icon consisting of a lowercase letter i in a circle appears in the message box.
  (:MB_ICONQUESTION		#x00000020)	;A question-mark icon appears in the message box. The question-mark message icon is no longer recommended because it does not clearly represent a specific type of message and because the phrasing of a message as a question could apply to any message type. In addition, users can confuse the message symbol question mark with Help information. Therefore, do not use this question mark message symbol in your message boxes. The system continues to support its inclusion only for backward compatibility.
  (:MB_ICONSTOP			#x00000010)	;A stop-sign icon appears in the message box.
  (:MB_ICONERROR		#x00000010)	;A stop-sign icon appears in the message box.
  (:MB_ICONHAND			#x00000010)	;A stop-sign icon appears in the message box.
  (:MB_DEFBUTTON1		#x00000000)	;The first button is the default button.MB_DEFBUTTON1 is the default unless MB_DEFBUTTON2, MB_DEFBUTTON3, or MB_DEFBUTTON4 is specified.
  (:MB_DEFBUTTON2		#x00000100)	;The second button is the default button.
  (:MB_DEFBUTTON3		#x00000200)	;The third button is the default button.
  (:MB_DEFBUTTON4		#x00000300)	;The fourth button is the default button.
  (:MB_APPLMODAL		#x00000000)	;The user must respond to the message box before continuing work in the window identified by the hWnd parameter. However, the user can move to the windows of other threads and work in those windows.Depending on the hierarchy of windows in the application, the user may be able to move to other windows within the thread. All child windows of the parent of the message box are automatically disabled, but pop-up windows are not.MB_APPLMODAL is the default if neither MB_SYSTEMMODAL nor MB_TASKMODAL is specified.
  (:MB_SYSTEMMODAL		#x00001000)	;Same as MB_APPLMODAL except that the message box has the WS_EX_TOPMOST style. Use system-modal message boxes to notify the user of serious, potentially damaging errors that require immediate attention (for example, running out of memory). This flag has no effect on the user's ability to interact with windows other than those associated with hWnd.
  (:MB_TASKMODAL		#x00002000)	;Same as MB_APPLMODAL except that all the top-level windows belonging to the current thread are disabled if the hWnd parameter is NULL. Use this flag when the calling application or library does not have a window handle available but still needs to prevent input to other windows in the calling thread without suspending other threads.
  (:MB_DEFAULT_DESKTOP_ONLY	#x00020000)	;Same as desktop of the interactive window station. For more information, see Window Stations.If the current input desktop is not the default desktop, MessageBox does not return until the user switches to the default desktop.
  (:MB_RIGHT			#x00080000)	;The text is right-justified.
  (:MB_RTLREADING		#x00100000)	;Displays message and caption text using right-to-left reading order on Hebrew and Arabic systems.
  (:MB_SETFOREGROUND		#x00010000)	;The message box becomes the foreground window. Internally, the system calls the SetForegroundWindow function for the message box.
  (:MB_TOPMOST			#x00040000)	;The message box is created with the WS_EX_TOPMOST window style.
  (:MB_SERVICE_NOTIFICATION	#x00200000)	;The caller is a service notifying the user of an event. The function displays a message box on the current active desktop, even if there is no user logged on to the computer.Terminal Services: If the calling thread has an impersonation token, the function directs the message box to the session specified in the impersonation token.If this flag is set, the hWnd parameter must be NULL. This is so that the message box can appear on a desktop other than the desktop corresponding to the hWnd.For information on security considerations in regard to using this flag, see Interactive Services. In particular, be aware that this flag can produce interactive content on a locked desktop and should therefore be used for only a very limited set of scenarios, such as resource exhaustion.
  )

(defcenum (MB_RESULT_ENUM :int)
  (:IDABORT 3)		;The Abort button was selected.
  (:IDCANCEL 2)		;The Cancel button was selected.
  (:IDCONTINUE 11)	;The Continue button was selected.
  (:IDIGNORE 5)		;The Ignore button was selected.
  (:IDNO 7)		;The No button was selected.
  (:IDOK 1)		;The OK button was selected.
  (:IDRETRY 4)		;The Retry button was selected.
  (:IDTRYAGAIN 10)	;The Try Again button was selected.
  (:IDYES 6)		;The Yes button was selected.
  )

(defbitfield (MONITOR_FLAG DWORD)
  (:MONITOR_DEFAULTTONULL       #x00000000)
  (:MONITOR_DEFAULTTOPRIMARY    #x00000001)
  (:MONITOR_DEFAULTTONEAREST    #x00000002)
  )

(defcstruct MONITORINFOEX
  (:cbSize DWORD)
  (:rcMonitor (:struct RECT))
  (:rcWork (:struct RECT))
  (:dwFlags DWORD)
  (:szDevice WORD :count 32))

(defbitfield (TH32CS_FLAG DWORD)
  (:TH32CS_INHERIT	#x80000000) ;Indicates that the snapshot handle is to be inheritable.
  (:TH32CS_SNAPHEAPLIST	#x00000001);Includes all heaps of the process specified in th32ProcessID in the snapshot. To enumerate the heaps, see Heap32ListFirst.
  (:TH32CS_SNAPMODULE	#x00000008);Includes all modules of the process specified in th32ProcessID in the snapshot. To enumerate the modules, see Module32First. If the function fails with ERROR_BAD_LENGTH, retry the function until it succeeds.64-bit Windows:  Using this flag in a 32-bit process includes the 32-bit modules of the process specified in th32ProcessID, while using it in a 64-bit process includes the 64-bit modules. To include the 32-bit modules of the process specified in th32ProcessID from a 64-bit process, use the TH32CS_SNAPMODULE32 flag.
  (:TH32CS_SNAPMODULE32	#x00000010);Includes all 32-bit modules of the process specified in th32ProcessID in the snapshot when called from a 64-bit process. This flag can be combined with TH32CS_SNAPMODULE or TH32CS_SNAPALL. If the function fails with ERROR_BAD_LENGTH, retry the function until it succeeds.
  (:TH32CS_SNAPPROCESS	#x00000002);Includes all processes in the system in the snapshot. To enumerate the processes, see Process32First.
  (:TH32CS_SNAPTHREAD	#x00000004));Includes all threads in the system in the snapshot. To enumerate the threads, see Thread32First.To identify the threads that belong to a specific process, compare its process identifier to the th32OwnerProcessID member of the THREADENTRY32 structure when enumerating the threads.

(defparameter +TH32CS_SNAPALL+ '(:TH32CS_SNAPHEAPLIST :TH32CS_SNAPMODULE :TH32CS_SNAPPROCESS :TH32CS_SNAPTHREAD));Includes all processes and threads in the system, plus the heaps and modules of the process specified in th32ProcessID. Equivalent to specifying the TH32CS_SNAPHEAPLIST, TH32CS_SNAPMODULE, TH32CS_SNAPPROCESS, and TH32CS_SNAPTHREAD values combined using an OR operation.

(defcstruct THREADENTRY32
  (:dwSize   DWORD)
  (:cntUsage DWORD)
  (:th32ThreadID DWORD)
  (:th32OwnerProcessID DWORD)
  (:tpBasePri  :long)
  (:tpDeltaPri :long)
  (:dwFlags DWORD))

(defcstruct PROCESSENTRY32
  (:dwSize DWORD)
  (:cntUsage DWORD)
  (:th32ProcessID DWORD)
  (:th32DefaultHeapID ULONG_PTR)
  (:th32ModuleID DWORD)
  (:cntThreads DWORD)
  (:th32ParentProcessID DWORD)
  (:pcPriClassBase :long)
  (:dwFlags DWORD)		
  (:szExeFile WORD :count 260))

(defcstruct MODULEENTRY32
  (:dwSize DWORD)
  (:th32ModuleID DWORD)
  (:th32ProcessID DWORD)
  (:GlblcntUsage DWORD)
  (:ProccntUsage DWORD)
  (:modBaseAddr (:pointer C_BYTE))
  (:modBaseSize DWORD)
  (:hModule HMODULE)
  (:szModule WORD :count 256)
  (:szExePath WORD :count 260))

(defcstruct HEAPENTRY32
  (:dwSize SIZE_T)
  (:hHandle HANDLE)
  (:dwAddress ULONG_PTR)
  (:dwBlockSize SIZE_T)
  (:dwFlags DWORD)
  (:dwLockCount DWORD)
  (:dwResvd DWORD)
  (:th32ProcessID DWORD)
  (:th32HeapID ULONG_PTR))

(defcstruct HEAPLIST32
  (:dwSize SIZE_T)
  (:th32ProcessID DWORD)
  (:th32HeapID ULONG_PTR)
  (:dwFlags DWORD))

(defbitfield (%THREAD_FLAG ACCESS_MASK)
  (:THREAD_DIRECT_IMPERSONATION		#x0200)		; Required for a server thread that impersonates a client.
  (:THREAD_GET_CONTEXT			#x0008)		; Required to read the context of a thread using GetThreadContext.
  (:THREAD_IMPERSONATE			#x0100)		; Required to use a thread's security information directly without calling it by using a communication mechanism that provides impersonation services.
  (:THREAD_QUERY_INFORMATION		#x0040)		; Required to read certain information from the thread object, such as the exit code (see GetExitCodeThread).
  (:THREAD_QUERY_LIMITED_INFORMATION	#x0800)		; Required to read certain information from the thread objects (see GetProcessIdOfThread). A handle that has the THREAD_QUERY_INFORMATION access right is automatically granted THREAD_QUERY_LIMITED_INFORMATION.Windows Server 2003 and Windows XP:  This access right is not supported.
  (:THREAD_SET_CONTEXT			#x0010)		; Required to write the context of a thread using SetThreadContext.
  (:THREAD_SET_INFORMATION		#x0020)		; Required to set certain information in the thread object.
  (:THREAD_SET_LIMITED_INFORMATION	#x0400)		; Required to set certain information in the thread object. A handle that has the THREAD_SET_INFORMATION access right is automatically granted THREAD_SET_LIMITED_INFORMATION.Windows Server 2003 and Windows XP:  This access right is not supported.
  (:THREAD_SET_THREAD_TOKEN		#x0080)		; Required to set the impersonation token for a thread using SetThreadToken.
  (:THREAD_SUSPEND_RESUME		#x0002)		; Required to suspend or resume a thread (see SuspendThread and ResumeThread).
  (:THREAD_TERMINATE			#x0001))	; Required to terminate a thread using TerminateThread.

(defctype THREAD_FLAG (bitfield-union ACCESS_MASK STANDARD_RIGHTS_FLAG %THREAD_FLAG))

(defparameter +THREAD_ALL_ACCESS+			;All possible access rights for a thread object.Windows Server 2003 and Windows XP:  The value of the THREAD_ALL_ACCESS flag increased on Windows Server 2008 and Windows Vista. If an application compiled for Windows Server 2008 and Windows Vista is run on Windows Server 2003 or Windows XP, the THREAD_ALL_ACCESS flag contains access bits that are not supported and the function specifying this flag fails with ERROR_ACCESS_DENIED. To avoid this problem, specify the minimum set of access rights required for the operation. If THREAD_ALL_ACCESS must be used, set _WIN32_WINNT to the minimum operating system targeted by your application (for example, #define _WIN32_WINNT _WIN32_WINNT_WINXP). For more information, see Using the Windows Headers.
  (list*
   :THREAD_DIRECT_IMPERSONATION		
   :THREAD_GET_CONTEXT			
   :THREAD_IMPERSONATE			
   :THREAD_QUERY_INFORMATION		
   :THREAD_QUERY_LIMITED_INFORMATION	
   :THREAD_SET_CONTEXT			
   :THREAD_SET_INFORMATION		
   :THREAD_SET_LIMITED_INFORMATION	
   :THREAD_SET_THREAD_TOKEN		
   :THREAD_SUSPEND_RESUME		
   :THREAD_TERMINATE			
   :SYNCHRONIZE   
   +STANDARD_RIGHTS_REQUIRED+))

(defbitfield (%PROCESSOR_FLAG ACCESS_MASK)
  (:PROCESS_CREATE_PROCESS		#x0080) ; Required to create a process.
  (:PROCESS_CREATE_THREAD		#x0002); Required to create a thread.
  (:PROCESS_DUP_HANDLE			#x0040); Required to duplicate a handle using DuplicateHandle.
  (:PROCESS_QUERY_INFORMATION		#x0400); Required to retrieve certain information about a process, such as its token, exit code, and priority class (see OpenProcessToken).
  (:PROCESS_QUERY_LIMITED_INFORMATION	#x1000); Required to retrieve certain information about a process (see GetExitCodeProcess, GetPriorityClass, IsProcessInJob, QueryFullProcessImageName). A handle that has the PROCESS_QUERY_INFORMATION access right is automatically granted PROCESS_QUERY_LIMITED_INFORMATION.Windows Server 2003 and Windows XP:  This access right is not supported.
  (:PROCESS_SET_INFORMATION		#x0200); Required to set certain information about a process, such as its priority class (see SetPriorityClass).
  (:PROCESS_SET_QUOTA			#x0100); Required to set memory limits using SetProcessWorkingSetSize.
  (:PROCESS_SUSPEND_RESUME		#x0800); Required to suspend or resume a process.
  (:PROCESS_TERMINATE			#x0001); Required to terminate a process using TerminateProcess.
  (:PROCESS_VM_OPERATION		#x0008); Required to perform an operation on the address space of a process (see VirtualProtectEx and WriteProcessMemory).
  (:PROCESS_VM_READ			#x0010); Required to read memory in a process using ReadProcessMemory.
  (:PROCESS_VM_WRITE			#x0020)); Required to write to memory in a process using WriteProcessMemory.

(defctype PROCESS_FLAG (bitfield-union ACCESS_MASK STANDARD_RIGHTS_FLAG %PROCESSOR_FLAG))

(defparameter +PROCESS_ALL_ACCESS+
  (list*
   :PROCESS_CREATE_PROCESS		
   :PROCESS_CREATE_THREAD
   :PROCESS_DUP_HANDLE			
   :PROCESS_QUERY_INFORMATION		
   :PROCESS_QUERY_LIMITED_INFORMATION	
   :PROCESS_SET_INFORMATION		
   :PROCESS_SET_QUOTA			
   :PROCESS_SUSPEND_RESUME		
   :PROCESS_TERMINATE			
   :PROCESS_VM_OPERATION		
   :PROCESS_VM_READ			
   :PROCESS_VM_WRITE
   :SYNCHRONIZE
   +STANDARD_RIGHTS_REQUIRED+
   ))

(defcenum (WAIT_RESULT_ENUM DWORD)
  (:WAIT_ABANDONED	#x00000080);The specified object is a mutex object that was not released by the thread that owned the mutex object before the owning thread terminated. Ownership of the mutex object is granted to the calling thread and the mutex state is set to nonsignaled.If the mutex was protecting persistent state information, you should check it for consistency.
  (:WAIT_IO_COMPLETION	#x000000C0);The wait was ended by one or more user-mode asynchronous procedure calls (APC) queued to the thread.
  (:WAIT_OBJECT_0	#x00000000);The state of the specified object is signaled.
  (:WAIT_TIMEOUT	#x00000102);The time-out interval elapsed, and the object's state is nonsignaled.
  (:WAIT_FAILED		#xFFFFFFF));The function has failed. To get extended error information, call GetLastError.

(defcenum (UOI_ENUM :int)
  (:UOI_FLAGS 1)	;The handle flags. The pvInfo parameter must point to a USEROBJECTFLAGS structure.
  (:UOI_HEAPSIZE 5)	;The size of the desktop heap, in KB, as a ULONG value. The hObj parameter must be a handle to a desktop object, otherwise, the function fails.Windows Server 2003 and Windows XP/2000:  This value is not supported.
  (:UOI_IO 6)		;TRUE if the hObj parameter is a handle to the desktop object that is receiving input from the user. FALSE otherwise.Windows Server 2003 and Windows XP/2000:  This value is not supported.
  (:UOI_NAME 2)		;The name of the object, as a string.
  (:UOI_TYPE 3)		;The type name of the object, as a string.
  (:UOI_USER_SID 4)     ;The SID structure that identifies the user that is currently associated with the specified object. If no user is associated with the object, the value returned in the buffer pointed to by lpnLengthNeeded is zero. Note that SID is a variable length structure. You will usually make a call to GetUserObjectInformation to determine the length of the SID before retrieving its value.
  (:UOI_TIMERPROC_EXCEPTION_SUPPRESSION 7)) ;for SetUserObjectInformationW

(defcstruct USEROBJECTFLAGS
  (:fInherit  :boolean)
  (:fReserved :boolean)
  (:dwFlags   DF_FLAG))
