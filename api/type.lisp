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
	   ACCEL
	   ACCEL_VIRT_FLAG
	   AW_FLAG
	   BS_FLAG
	   COMPUTER_NAME_FORMAT_ENUM
	   CS_FLAG
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
	   HACCEL
	   HBRUSH
	   HCURSOR
	   HDC
	   HDESK
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
	   LOWORD
	   LPARAM
	   LPVOID
	   LRESULT
	   MSG
	   OSVERSIONINFOEX
	   PAINTSTRUCT
	   POINT
	   PRODUCT_ENUM
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
	   WNDCLASSEX
	   WND_MESSAGE
	   WND_STYLE
	   WORD
	   WPARAM
	   WS_EX_FLAG
	   WS_FLAG
	   enum-union
	   ))

(in-package #:w32api.type)

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

(defctype HWINSTA	:pointer)
(defctype HDESK		:pointer)
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

;;; CreateDesktop dwFlags

(defbitfield (DF_FLAG DWORD)
  (:DF_ALLOWOTHERACCOUNTHOOK #x0001))		;Enables processes running in other accounts on the desktop to set hooks in this process.

(defbitfield (DA_FLAG ACCESS_MASK)
  (:DELETE			#x00010000)	;Required to delete the object.
  (:READ_CONTROL		#x00020000)	;Required to read information in the security descriptor for the object, not including the information in the SACL. To read or write the SACL, you must request the ACCESS_SYSTEM_SECURITY access right. For more information, see SACL Access Right.
  (:SYNCHRONIZE			#x00100000)	;Not supported for desktop objects.
  (:WRITE_DAC			#x00040000)	;Required to modify the DACL in the security descriptor for the object.
  (:WRITE_OWNER			#x00080000)	;Required to change the owner in the security descriptor for the object.
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

(defparameter +STANDARD_RIGHTS_ALL+
  '(:DELETE
    :READ_CONTROL
    :WRITE_DAC
    :WRITE_OWNER
    :SYNCHRONIZE))

(defparameter +STANDARD_RIGHTS_EXECUTE+
  :READ_CONTROL)

(defparameter +STANDARD_RIGHTS_READ+
  :READ_CONTROL)

(defparameter +STANDARD_RIGHTS_REQUIRED+
  '(:DELETE
    :READ_CONTROL
    :WRITE_DAC
    :WRITE_OWNER))

(defparameter +STANDARD_RIGHTS_WRITE+
  :READ_CONTROL)

(defparameter +DESKTOP_GENERIC_READ+
  '(:DESKTOP_ENUMERATE
    :DESKTOP_READOBJECTS
    +STANDARD_RIGHTS_READ+))

(defparameter +DESKTOP_GENERIC_WRITE+
  '(:DESKTOP_CREATEMENU
    :DESKTOP_CREATEWINDOW
    :DESKTOP_HOOKCONTROL
    :DESKTOP_JOURNALPLAYBACK
    :DESKTOP_JOURNALRECORD
    :DESKTOP_WRITEOBJECTS
    +STANDARD_RIGHTS_WRITE+))

(defparameter +DESKTOP_GENERIC_EXECUTE+
  '(:DESKTOP_SWITCHDESKTOP +STANDARD_RIGHTS_EXECUTE+))

(defparameter +DESKTOP_GENERIC_ALL+
  (append '(:DESKTOP_CREATEMENU
	    :DESKTOP_CREATEWINDOW
	    :DESKTOP_ENUMERATE
	    :DESKTOP_HOOKCONTROL
	    :DESKTOP_JOURNALPLAYBACK
	    :DESKTOP_JOURNALRECORD
	    :DESKTOP_READOBJECTS
	    :DESKTOP_SWITCHDESKTOP
	    :DESKTOP_WRITEOBJECTS
	    ) +STANDARD_RIGHTS_REQUIRED+))

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

;;; SetClassLongPtr nIndex
(defcenum (GCL_ENUM :int)
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

(defcstruct POINT
  (:x :long)
  (:y :long))

(defcstruct MSG
  (:hwnd    HWND)
  (:message :uint)
  (:wParam  WPARAM)
  (:lParam  LPARAM)
  (:time    DWORD)
  (:pt      (:struct POINT)))

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

(defctype WND_MESSAGE (enum-union :uint WM_ENUM BM_ENUM))

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
