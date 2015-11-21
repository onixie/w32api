(defpackage #:w32api.type
  (:use #:common-lisp #:cffi #:w32api.util)
  (:export bitfield-union
	   LONG_PTR
	   ULONG_PTR
	   INT_PTR
	   UINT_PTR

	   C_BYTE
	   WORD
	   DWORD

	   HWINSTA
	   HDESK
	   HWND
	   HDC
	   HINSTANCE
	   HMODULE
	   HICON
	   HCURSOR
	   HBRUSH
	   HMENU
	   HACCEL

	   DF_FLAG
	   DA_FLAG
	   SECURITY_ATTRIBUTES
	   nLength
	   lpSecurityDescriptor
	   bInheritHandle

	   DLGPROC

	   LRESULT
	   LPARAM
	   WPARAM
	   C_ATOM


	   WNDCLASSEX
	   cbSize
	   style
	   lpfnWndProc
	   cbClsExtra       
	   cbWndExtra      
	   hInstance
	   hIcon
	   hCursor
	   hbrBackground
	   lpszMenuName
	   lpszClassName
	   hIconSm

	   GCL_ENUM
	   CS_FLAG

	   +CW_USEDEFAULT+
	   +HWND_MESSAGE+

	   WS_EX_FLAG
	   +WS_EX_OVERLAPPEDWINDOW+
	   +WS_EX_PALETTEWINDOW+

	   AW_FLAG

	   GW_ENUM

	   SW_ENUM

	   WS_FLAG
	   +WS_TILEDWINDOW+
	   +WS_POPUPWINDOW+
	   +WS_OVERLAPPEDWINDOW+

	   BS_FLAG
	   GWLP_ENUM
	   +DWLP_MSGRESULT+
	   +DWLP_DLGPROC+
	   +DWLP_USER+

	   ACCEL
	   fVirt
	   key
	   cmd

	   ACCEL_VIRT_FLAG
	   MSG
	   hwnd   
	   message
	   wParam 
	   lParam 
	   time   
	   pt

	   POINT
	   x
	   y
	   RECT
	   PAINTSTRUCT

	   +HWND_BROADCAST+	   

	   WM_ENUM
	   GA_ENUM

	   DEVMODE
	   ))

(in-package #:w32api.type)

(setf *default-foreign-encoding* :utf-16le)

#+x86-64
(progn
  (defctype INT_PTR :int64)
  (defctype UINT_PTR :uint64)
  (defctype LONG_PTR :int64)
  (defctype ULONG_PTR :uint64))
#+x86
(progn
  (defctype INT_PTR :int32)
  (defctype UINT_PTR :uint32)
  (defctype LONG_PTR :int32)
  (defctype ULONG_PTR :uint32))

(defctype C_BYTE :unsigned-char)
(defctype WORD :unsigned-short)
(defctype DWORD :unsigned-long)

(defctype HWINSTA :pointer)
(defctype HDESK :pointer)
(defctype HWND :pointer)
(defctype HDC  :pointer)
(defctype HINSTANCE  :pointer)
(defctype HMODULE  :pointer)
(defctype HICON  :pointer)
(defctype HCURSOR  :pointer)
(defctype HBRUSH  :pointer)
(defctype HMENU :pointer)
(defctype HACCEL :pointer)

(defctype LRESULT LONG_PTR)
(defctype LPARAM LONG_PTR)
(defctype WPARAM UINT_PTR)
(defctype C_ATOM WORD)

(defctype ACCESS_MASK DWORD)

;;; CreateDesktop dwFlags

(defbitfield (DF_FLAG DWORD)
  (:ALLOWOTHERACCOUNTHOOK #x0001));Enables processes running in other accounts on the desktop to set hooks in this process.

(defbitfield (DA_FLAG ACCESS_MASK)
  (:DELETE #x00010000);	Required to delete the object.
  (:READ_CONTROL #x00020000) ;Required to read information in the security descriptor for the object, not including the information in the SACL. To read or write the SACL, you must request the ACCESS_SYSTEM_SECURITY access right. For more information, see SACL Access Right.
  (:SYNCHRONIZE #x00100000)  ;Not supported for desktop objects.
  (:WRITE_DAC #x00040000);Required to modify the DACL in the security descriptor for the object.
  (:WRITE_OWNER #x00080000);Required to change the owner in the security descriptor for the object.
  (:CREATEMENU #x0004)	;Required to create a menu on the desktop.
  (:CREATEWINDOW #x0002) ;Required to create a window on the desktop.
  (:ENUMERATE #x0040)	;Required for the desktop to be enumerated.
  (:HOOKCONTROL #x0008)	;Required to establish any of the window hooks.
  (:JOURNALPLAYBACK #x0020) ;Required to perform journal playback on a desktop.
  (:JOURNALRECORD #x0010) ;Required to perform journal recording on a desktop.
  (:READOBJECTS #x0001)	;Required to read objects on the desktop.
  (:SWITCHDESKTOP #x0100) ;Required to activate the desktop using the SwitchDesktop function.
  (:WRITEOBJECTS #x0080) ;Required to write objects on the desktop.
  ) 

(defparameter +STANDARD_RIGHTS_ALL+
  '(:DELETE :READ_CONTROL :WRITE_DAC :WRITE_OWNER :SYNCHRONIZE))
(defparameter +STANDARD_RIGHTS_EXECUTE+
  :READ_CONTROL)
(defparameter +STANDARD_RIGHTS_READ+
  :READ_CONTROL)
(defparameter +STANDARD_RIGHTS_REQUIRED+
  '(:DELETE :READ_CONTROL :WRITE_DAC :WRITE_OWNER))
(defparameter +STANDARD_RIGHTS_WRITE+
  :READ_CONTROL)

(defparameter +DESKTOP_GENERIC_READ+	
  '(:ENUMERATE
    :READOBJECTS
    +STANDARD_RIGHTS_READ+))
(defparameter +DESKTOP_GENERIC_WRITE+	
  '(:CREATEMENU
    :CREATEWINDOW
    :HOOKCONTROL
    :JOURNALPLAYBACK
    :JOURNALRECORD
    :WRITEOBJECTS
    +STANDARD_RIGHTS_WRITE+))
(defparameter +DESKTOP_GENERIC_EXECUTE+	
  '(:SWITCHDESKTOP +STANDARD_RIGHTS_EXECUTE+))
(defparameter +DESKTOP_GENERIC_ALL+	
  (append '(:CREATEMENU
	    :CREATEWINDOW
	    :ENUMERATE
	    :HOOKCONTROL
	    :JOURNALPLAYBACK
	    :JOURNALRECORD
	    :READOBJECTS
	    :SWITCHDESKTOP
	    :WRITEOBJECTS
	    ) +STANDARD_RIGHTS_REQUIRED+))


(defcstruct SECURITY_ATTRIBUTES
  (nLength  DWORD)
  (lpSecurityDescriptor (:pointer :void))
  (bInheritHandle   :boolean)
  )

;;; 
(defctype DLGPROC :pointer)

(defparameter +CW_USEDEFAULT+ (- 0 #x80000000))

(defparameter +HWND_MESSAGE+ -3)

(defbitfield (CS_FLAG :unsigned-int)
  (:BYTEALIGNCLIENT #x1000); Aligns the window's client area on a byte boundary (in the x direction). This style affects the width of the window and its horizontal placement on the display.
  (:BYTEALIGNWINDOW #x2000); Aligns the window on a byte boundary (in the x direction). This style affects the width of the window and its horizontal placement on the display.
  (:CLASSDC #x0040); Allocates one device context to be shared by all windows in the class. Because window classes are process specific, it is possible for multiple threads of an application to create a window of the same class. It is also possible for the threads to attempt to use the device context simultaneously. When this happens, the system allows only one thread to successfully finish its drawing operation.
  (:DBLCLKS #x0008); Sends a double-click message to the window procedure when the user double-clicks the mouse while the cursor is within a window belonging to the class.
  (:DROPSHADOW #x00020000); Enables the drop shadow effect on a window. The effect is turned on and off through SPI_SETDROPSHADOW. Typically, this is enabled for small, short-lived windows such as menus to emphasize their Z-order relationship to other windows. Windows created from a class with this style must be top-level windows; they may not be child windows.
  (:GLOBALCLASS #x4000); Indicates that the window class is an application global class. For more information, see the "Application Global Classes" section of About Window Classes.
  (:HREDRAW #x0002); Redraws the entire window if a movement or size adjustment changes the width of the client area.
  (:NOCLOSE #x0200); Disables Close on the window menu.
  (:OWNDC #x0020); Allocates a unique device context for each window in the class.
  (:PARENTDC #x0080); Sets the clipping rectangle of the child window to that of the parent window so that the child can draw on the parent. A window with the CS_PARENTDC style bit receives a regular device context from the system's cache of device contexts. It does not give the child the parent's device context or device context settings. Specifying CS_PARENTDC enhances an application's performance.
  (:SAVEBITS #x0800); Saves, as a bitmap, the portion of the screen image obscured by a window of this class. When the window is removed, the system uses the saved bitmap to restore the screen image, including other windows that were obscured. Therefore, the system does not send WM_PAINT messages to windows that were obscured if the memory used by the bitmap has not been discarded and if other screen actions have not invalidated the stored image.This style is useful for small windows (for example, menus or dialog boxes) that are displayed briefly and then removed before other screen activity takes place. This style increases the time required to display the window, because the system must first allocate memory to store the bitmap.
  (:VREDRAW #x0001)); Redraws the entire window if a movement or size adjustment changes the height of the client area.

(defbitfield (BS_FLAG DWORD)
  (:PUSHBUTTON       #x00000000)
  (:DEFPUSHBUTTON    #x00000001)
  (:CHECKBOX         #x00000002)
  (:AUTOCHECKBOX     #x00000003)
  (:RADIOBUTTON      #x00000004)
  (:3STATE           #x00000005)
  (:AUTO3STATE       #x00000006)
  (:GROUPBOX         #x00000007)
  (:USERBUTTON       #x00000008)
  (:AUTORADIOBUTTON  #x00000009)
  (:PUSHBOX          #x0000000A)
  (:OWNERDRAW        #x0000000B)
  (:TYPEMASK         #x0000000F)
  (:LEFTTEXT         #x00000020)

  (:TEXT             #x00000000);;below is for winver > 4
  (:ICON             #x00000040)
  (:BITMAP           #x00000080)
  (:LEFT             #x00000100)
  (:RIGHT            #x00000200)
  (:CENTER           #x00000300)
  (:TOP              #x00000400)
  (:BOTTOM           #x00000800)
  (:VCENTER          #x00000C00)
  (:PUSHLIKE         #x00001000)
  (:MULTILINE        #x00002000)
  (:NOTIFY           #x00004000)
  (:FLAT             #x00008000)
  )

;;; SetClassLongPtr nIndex
(defcenum (GCL_ENUM :int)
  (:CBCLSEXTRA -20);Sets the size, in bytes, of the extra memory associated with the class. Setting this value does not change the number of extra bytes already allocated.
  (:CBWNDEXTRA -18);Sets the size, in bytes, of the extra window memory associated with each window in the class. Setting this value does not change the number of extra bytes already allocated. For information on how to access this memory, see SetWindowLongPtr.
  (:HBRBACKGROUND -10);Replaces a handle to the background brush associated with the class.
  (:HCURSOR -12);Replaces a handle to the cursor associated with the class.
  (:HICON -14);Replaces a handle to the icon associated with the class.
  (:HICONSM -34);Retrieves a handle to the small icon associated with the class.
  (:HMODULE -16);Replaces a handle to the module that registered the class.
  (:MENUNAME -8);Replaces the pointer to the menu name string. The string identifies the menu resource associated with the class.
  (:STYLE -26);Replaces the window-class style bits.
  (:WNDPROC -24));Replaces the pointer to the window procedure associated with the class.

(defcstruct WNDCLASSEX
  (cbSize  :unsigned-int)
  (style      CS_FLAG)
  (lpfnWndProc   :pointer)
  (cbClsExtra       :int)
  (cbWndExtra       :int)
  (hInstance HINSTANCE)
  (hIcon     HICON)
  (hCursor   HCURSOR)
  (hbrBackground    HBRUSH)
  (lpszMenuName   :string)
  (lpszClassName   :string)
  (hIconSm     HICON)
  )

;; Extended Window Styles
(defbitfield (WS_EX_FLAG DWORD)
  (:ACCEPTFILES #X00000010);The window accepts drag-drop files.
  (:APPWINDOW #X00040000);Forces a top-level window onto the taskbar when the window is visible.
  (:CLIENTEDGE #X00000200);The window has a border with a sunken edge.
  (:COMPOSITED #X02000000);Paints all descendants of a window in bottom-to-top painting order using double-buffering. For more information, see Remarks. This cannot be used if the window has a class style of either CS_OWNDC or CS_CLASSDC. Windows 2000:  This style is not supported.
  (:CONTEXTHELP #X00000400);The title bar of the window includes a question mark. When the user clicks the question mark, the cursor changes to a question mark with a pointer. If the user then clicks a child window, the child receives a WM_HELP message. The child window should pass the message to the parent window procedure, which should call the WinHelp function using the HELP_WM_HELP command. The Help application displays a pop-up window that typically contains help for the child window.WS_EX_CONTEXTHELP cannot be used with the WS_MAXIMIZEBOX or WS_MINIMIZEBOX styles.
  (:CONTROLPARENT #X00010000);The window itself contains child windows that should take part in dialog box navigation. If this style is specified, the dialog manager recurses into children of this window when performing navigation operations such as handling the TAB key, an arrow key, or a keyboard mnemonic.
  (:DLGMODALFRAME #X00000001);The window has a double border; the window can, optionally, be created with a title bar by specifying the WS_CAPTION style in the dwStyle parameter.
  (:LAYERED #X0008000);The window is a layered window. This style cannot be used if the window has a class style of either CS_OWNDC or CS_CLASSDC.Windows 8:  The WS_EX_LAYERED style is supported for top-level windows and child windows. Previous Windows versions support WS_EX_LAYERED only for top-level windows.
  (:LAYOUTRTL #X00400000);If the shell language is Hebrew, Arabic, or another language that supports reading order alignment, the horizontal origin of the window is on the right edge. Increasing horizontal values advance to the left.
  (:LEFT #X00000000);The window has generic left-aligned properties. This is the default.
  (:LEFTSCROLLBAR #X00004000);If the shell language is Hebrew, Arabic, or another language that supports reading order alignment, the vertical scroll bar (if present) is to the left of the client area. For other languages, the style is ignored.
  (:LTRREADING #X00000000);The window text is displayed using left-to-right reading-order properties. This is the default.
  (:MDICHILD #X00000040);The window is a MDI child window.
  (:NOACTIVATE #X08000000);A top-level window created with this style does not become the foreground window when the user clicks it. The system does not bring this window to the foreground when the user minimizes or closes the foreground window.To activate the window, use the SetActiveWindow or SetForegroundWindow function.The window does not appear on the taskbar by default. To force the window to appear on the taskbar, use the WS_EX_APPWINDOW style.
  (:NOINHERITLAYOUT #X00100000);The window does not pass its window layout to its child windows.
  (:NOPARENTNOTIFY #X00000004);The child window created with this style does not send the WM_PARENTNOTIFY message to its parent window when it is created or destroyed.
  (:NOREDIRECTIONBITMAP #X00200000);The window does not render to a redirection surface. This is for windows that do not have visible content or that use mechanisms other than surfaces to provide their visual.
  (:RIGHT #X00001000);The window has generic "right-aligned" properties. This depends on the window class. This style has an effect only if the shell language is Hebrew, Arabic, or another language that supports reading-order alignment; otherwise, the style is ignored. Using the WS_EX_RIGHT style for static or edit controls has the same effect as using the SS_RIGHT or ES_RIGHT style, respectively. Using this style with button controls has the same effect as using BS_RIGHT and BS_RIGHTBUTTON styles.
  (:RIGHTSCROLLBAR #X00000000);The vertical scroll bar (if present) is to the right of the client area. This is the default.
  (:RTLREADING #X00002000);If the shell language is Hebrew, Arabic, or another language that supports reading-order alignment, the window text is displayed using right-to-left reading-order properties. For other languages, the style is ignored.
  (:STATICEDGE #X00020000);The window has a three-dimensional border style intended to be used for items that do not accept user input.
  (:TOOLWINDOW #X00000080);The window is intended to be used as a floating toolbar. A tool window has a title bar that is shorter than a normal title bar, and the window title is drawn using a smaller font. A tool window does not appear in the taskbar or in the dialog that appears when the user presses ALT+TAB. If a tool window has a system menu, its icon is not displayed on the title bar. However, you can display the system menu by right-clicking or by typing ALT+SPACE.
  (:TOPMOST #X00000008);The window should be placed above all non-topmost windows and should stay above them, even when the window is deactivated. To add or remove this style, use the SetWindowPos function.
  (:TRANSPARENT #X00000020);The window should not be painted until siblings beneath the window (that were created by the same thread) have been painted. The window appears transparent because the bits of underlying sibling windows have already been painted. To achieve transparency without these restrictions, use the SetWindowRgn function.
  (:WINDOWEDGE #X00000100);The window has a border with a raised edge.
  )

(defparameter +WS_EX_OVERLAPPEDWINDOW+ ;The window is an overlapped window.
  '(:WINDOWEDGE
    :CLIENTEDGE))		  

(defparameter +WS_EX_PALETTEWINDOW+ ;The window is palette window, which is a modeless dialog box that presents an array of commands.
  '(:WINDOWEDGE
    :TOOLWINDOW
    :TOPMOST))

;;; Window Style
(defbitfield (WS_FLAG DWORD)
  (:BORDER #x00800000)	   ;The window has a thin-line border.
  (:CAPTION #x00C00000) ;The window has a title bar (includes the WS_BORDER style).
  (:CHILD #x40000000) ;The window is a child window. A window with this style cannot have a menu bar. This style cannot be used with the WS_POPUP style.
  (:CHILDWINDOW #x40000000)		;Same as the WS_CHILD style.
  (:CLIPCHILDREN #x02000000) ;Excludes the area occupied by child windows when drawing occurs within the parent window. This style is used when creating the parent window.
  (:CLIPSIBLINGS #x04000000) ;Clips child windows relative to each other; that is, when a particular child window receives a WM_PAINT message, the WS_CLIPSIBLINGS style clips all other overlapping child windows out of the region of the child window to be updated. If WS_CLIPSIBLINGS is not specified and child windows overlap, it is possible, when drawing within the client area of a child window, to draw within the client area of a neighboring child window.
  (:DISABLED #x08000000) ;The window is initially disabled. A disabled window cannot receive input from the user. To change this after a window has been created, use the EnableWindow function.
  (:DLGFRAME #x00400000) ;The window has a border of a style typically used with dialog boxes. A window with this style cannot have a title bar.
  (:GROUP #x00020000) ;The window is the first control of a group of controls. The group consists of this first control and all controls defined after it, up to the next control with the WS_GROUP style. The first control in each group usually has the WS_TABSTOP style so that the user can move from group to group. The user can subsequently change the keyboard focus from one control in the group to the next control in the group by using the direction keys.You can turn this style on and off to change dialog box navigation. To change this style after a window has been created, use the SetWindowLong function.
  (:HSCROLL #x00100000)     ;The window has a horizontal scroll bar.
  (:ICONIC #x20000000) ;The window is initially minimized. Same as the WS_MINIMIZE style.
  (:MAXIMIZE #x01000000)	   ;The window is initially maximized.
  (:MAXIMIZEBOX #x00010000) ;The window has a maximize button. Cannot be combined with the WS_EX_CONTEXTHELP style. The WS_SYSMENU style must also be specified.
  (:MINIMIZE #x20000000) ;The window is initially minimized. Same as the WS_ICONIC style.
  (:MINIMIZEBOX #x00020000) ;The window has a minimize button. Cannot be combined with the WS_EX_CONTEXTHELP style. The WS_SYSMENU style must also be specified.
  (:OVERLAPPED #x00000000) ;The window is an overlapped window. An overlapped window has a title bar and a border. Same as the WS_TILED style.
  (:POPUP #x80000000) ;The windows is a pop-up window. This style cannot be used with the WS_CHILD style.
  (:SIZEBOX #x00040000) ;The window has a sizing border. Same as the WS_THICKFRAME style.
  (:SYSMENU #x00080000) ;The window has a window menu on its title bar. The WS_CAPTION style must also be specified.
  (:TABSTOP #x00010000) ;The window is a control that can receive the keyboard focus when the user presses the TAB key. Pressing the TAB key changes the keyboard focus to the next control with the WS_TABSTOP style.You can turn this style on and off to change dialog box navigation. To change this style after a window has been created, use the SetWindowLong function. For user-created windows and modeless dialogs to work with tab stops, alter the message loop to call the IsDialogMessage function.
  (:THICKFRAME #x00040000) ;The window has a sizing border. Same as the WS_SIZEBOX style.
  (:TILED #x00000000);The window is an overlapped window. An overlapped window has a title bar and a border. Same as the WS_OVERLAPPED style.
  (:VISIBLE #x10000000);The window is initially visible.This style can be turned on and off by using the ShowWindow or SetWindowPos function.
  (:VSCROLL #x00200000));The window has a vertical scroll bar.

(defparameter +WS_OVERLAPPEDWINDOW+;The window is an overlapped window. Same as the WS_TILEDWINDOW style.
  '(:OVERLAPPED
    :CAPTION
    :SYSMENU
    :THICKFRAME
    :MINIMIZEBOX
    :MAXIMIZEBOX))

(defparameter +WS_POPUPWINDOW+;The window is a pop-up window. The WS_CAPTION and WS_POPUPWINDOW styles must be combined to make the window menu visible.
  '(:POPUP
    :BORDER
    :SYSMENU))

(defparameter +WS_TILEDWINDOW+;The window is an overlapped window. Same as the WS_OVERLAPPEDWINDOW style.
  '(:OVERLAPPED
    :CAPTION  
    :SYSMENU
    :THICKFRAME
    :MINIMIZEBOX
    :MAXIMIZEBOX))

;;; GetModuleHandleEx dwFlags
(defbitfield (GET_MODULE_HANDLE_EX_FLAG DWORD);This parameter can be zero or one or more of the following values. If the module's reference count is incremented, the caller must use the FreeLibrary function to decrement the reference count when the module handle is no longer needed.
  (:FROM_ADDRESS #x00000004) ;The lpModuleName parameter is an address in the module.
  (:PIN #x00000001);The module stays loaded until the process is terminated, no matter how many times FreeLibrary is called.This option cannot be used with GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT.
  (:UNCHANGED_REFCOUNT #x00000002);The reference count for the module is not incremented. This option is equivalent to the behavior of GetModuleHandle. Do not pass the retrieved module handle to the FreeLibrary function; doing so can cause the DLL to be unmapped prematurely. For more information, see Remarks. This option cannot be used with GET_MODULE_HANDLE_EX_FLAG_PIN.
  )

(defcenum (GW_ENUM :uint)
  (:CHILD 5);The retrieved handle identifies the child window at the top of the Z order, if the specified window is a parent window; otherwise, the retrieved handle is NULL. The function examines only child windows of the specified window. It does not examine descendant windows.
  (:ENABLEDPOPUP 6);The retrieved handle identifies the enabled popup window owned by the specified window (the search uses the first such window found using GW_HWNDNEXT); otherwise, if there are no enabled popup windows, the retrieved handle is that of the specified window.
  (:HWNDFIRST 0);The retrieved handle identifies the window of the same type that is highest in the Z order.If the specified window is a topmost window, the handle identifies a topmost window. If the specified window is a top-level window, the handle identifies a top-level window. If the specified window is a child window, the handle identifies a sibling window.
  (:HWNDLAST 1);The retrieved handle identifies the window of the same type that is lowest in the Z order.If the specified window is a topmost window, the handle identifies a topmost window. If the specified window is a top-level window, the handle identifies a top-level window. If the specified window is a child window, the handle identifies a sibling window.
  (:HWNDNEXT 2);The retrieved handle identifies the window below the specified window in the Z order.If the specified window is a topmost window, the handle identifies a topmost window. If the specified window is a top-level window, the handle identifies a top-level window. If the specified window is a child window, the handle identifies a sibling window.
  (:HWNDPREV 3);The retrieved handle identifies the window above the specified window in the Z order.If the specified window is a topmost window, the handle identifies a topmost window. If the specified window is a top-level window, the handle identifies a top-level window. If the specified window is a child window, the handle identifies a sibling window.
  (:OWNER 4));The retrieved handle identifies the specified window's owner window, if any. For more information, see Owned Windows.

;;; ShowWindow nCmdShow 
(defcenum (SW_ENUM :int)
  (:FORCEMINIMIZE 11);Minimizes a window, even if the thread that owns the window is not responding. This flag should only be used when minimizing windows from a different thread.
  (:HIDE 0) ;Hides the window and activates another 
  (:MAXIMIZE 3) ;Maximizes the specified window.
  (:MINIMIZE 6) ;Minimizes the specified window and activates the next top-level window in the Z order.
  (:RESTORE 9) ;Activates and displays the window. If the window is minimized or maximized, the system restores it to its original size and position. An application should specify this flag when restoring a minimized window.
  (:SHOW 5) ;Activates the window and displays it in its current size and position.
  (:SHOWDEFAULT 10) ;Sets the show state based on the SW_ value specified in the STARTUPINFO structure passed to the CreateProcess function by the program that started the application.
  (:SHOWMAXIMIZED 3) ;Activates the window and displays it as a maximized window.
  (:SHOWMINIMIZED 2) ;Activates the window and displays it as a minimized window.
  (:SHOWMINNOACTIVE 7) ;Displays the window as a minimized window. This value is similar to SW_SHOWMINIMIZED, except the window is not activated.
  (:SHOWNA 8) ;Displays the window in its current size and position. This value is similar to SW_SHOW, except that the window is not activated.
  (:SHOWNOACTIVATE 4) ;Displays a window in its most recent size and position. This value is similar to SW_SHOWNORMAL, except that the window is not activated.
  (:SHOWNORMAL 1)) ;Activates and displays a window. If the window is minimized or maximized, the system restores it to its original size and position. An application should specify this flag when displaying the window for the first 

(defbitfield (AW_FLAG DWORD)
  (:ACTIVATE #x00020000);Activates the window. Do not use this value with AW_HIDE.
  (:BLEND #x00080000);Uses a fade effect. This flag can be used only if hwnd is a top-level window.
  (:CENTER #x00000010);Makes the window appear to collapse inward if AW_HIDE is used or expand outward if the AW_HIDE is not used. The various direction flags have no effect.
  (:HIDE #x00010000);Hides the window. By default, the window is shown.
  (:HOR_POSITIVE #x00000001);Animates the window from left to right. This flag can be used with roll or slide animation. It is ignored when used with AW_CENTER or AW_BLEND.
  (:HOR_NEGATIVE #x00000002);Animates the window from right to left. This flag can be used with roll or slide animation. It is ignored when used with AW_CENTER or AW_BLEND.
  (:SLIDE #x00040000);Uses slide animation. By default, roll animation is used. This flag is ignored when used with AW_CENTER.
  (:VER_POSITIVE #x00000004);Animates the window from top to bottom. This flag can be used with roll or slide animation. It is ignored when used with AW_CENTER or AW_BLEND.
  (:VER_NEGATIVE #x00000008);Animates the window from bottom to top. This flag can be used with roll or slide animation. It is ignored when used with AW_CENTER or AW_BLEND.
  )

;;; SetWindowLongPtr nIndex
(defcenum (GWLP_ENUM :int)
  (:EXSTYLE -20)	      ;Sets a new extended window style.
  (:HINSTANCE -6);Sets a new application instance handle.
  (:ID -12) ;Sets a new identifier of the child window. The window cannot be a top-level window.
  (:STYLE -16);Sets a new window style.
  (:USERDATA -21);Sets the user data associated with the window. This data is intended for use by the application that created the window. Its value is initially zero.
  (:GWLP_WNDPROC -4));Sets a new address for the window procedure.

(defparameter +DWLP_MSGRESULT+ 0);Sets the return value of a message processed in the dialog box procedure.

(defparameter +DWLP_DLGPROC+ 
  (+ +DWLP_MSGRESULT+ (foreign-type-size :pointer))) ;Sets the new pointer to the dialog box procedure.

(defparameter +DWLP_USER+
  (+ +DWLP_DLGPROC+  (foreign-type-size :pointer)))

(defcstruct ACCEL
  (fVirt C_BYTE)
  (key WORD)
  (cmd WORD))

(defbitfield (ACCEL_VIRT_FLAG C_BYTE)
  (:ALT #x10);The ALT key must be held down when the accelerator key is pressed.
  (:CONTROL #x08);The CTRL key must be held down when the accelerator key is pressed.
  (:NOINVERT #x02);No top-level menu item is highlighted when the accelerator is used. If this flag is not specified, a top-level menu item will be highlighted, if possible, when the accelerator is used. This attribute is obsolete and retained only for backward compatibility with resource files designed for 16-bit Windows.
  (:SHIFT #x04);The SHIFT key must be held down when the accelerator key is pressed.
  (:VIRTKEY #x01);The key member specifies a virtual-key code. If this flag is not specified, key is assumed to specify a character code.
  )

(defcstruct POINT
  (x :long)
  (y :long))

(defcstruct MSG 
  (hwnd        HWND)
  (message       :unsigned-int)
  (wParam      WPARAM)
  (lParam      LPARAM)
  (time       DWORD)
  (pt       (:struct POINT)))

(defparameter +HWND_BROADCAST+ #xffff);The message is posted to all top-level windows in the system, including disabled or invisible unowned windows, overlapped windows, and pop-up windows. The message is not posted to child windows.

(defcenum (WM_ENUM :unsigned-int)
  (:NULL                         #x0000)
  (:CREATE                       #x0001)
  (:DESTROY                      #x0002)
  (:MOVE                         #x0003)
  (:SIZE                         #x0005)
  (:ACTIVATE                     #x0006)
  (:SETFOCUS                     #x0007)
  (:KILLFOCUS                    #x0008)
  (:ENABLE                       #x000A)
  (:SETREDRAW                    #x000B)
  (:SETTEXT                      #x000C)
  (:GETTEXT                      #x000D)
  (:GETTEXTLENGTH                #x000E)
  (:PAINT                        #x000F)
  (:CLOSE                        #x0010)
  (:QUERYENDSESSION              #x0011)
  (:QUERYOPEN                    #x0013)
  (:ENDSESSION                   #x0016)
  (:QUIT                         #x0012)
  (:ERASEBKGND                   #x0014)
  (:SYSCOLORCHANGE               #x0015)
  (:SHOWWINDOW                   #x0018)
  (:SETTINGCHANGE                #x001A)
  (:DEVMODECHANGE                #x001B)
  (:ACTIVATEAPP                  #x001C)
  (:FONTCHANGE                   #x001D)
  (:TIMECHANGE                   #x001E)
  (:CANCELMODE                   #x001F)
  (:SETCURSOR                    #x0020)
  (:MOUSEACTIVATE                #x0021)
  (:CHILDACTIVATE                #x0022)
  (:QUEUESYNC                    #x0023)
  (:GETMINMAXINFO                #x0024)
  (:PAINTICON                    #x0026)
  (:ICONERASEBKGND               #x0027)
  (:NEXTDLGCTL                   #x0028)
  (:SPOOLERSTATUS                #x002A)
  (:DRAWITEM                     #x002B)
  (:MEASUREITEM                  #x002C)
  (:DELETEITEM                   #x002D)
  (:VKEYTOITEM                   #x002E)
  (:CHARTOITEM                   #x002F)
  (:SETFONT                      #x0030)
  (:GETFONT                      #x0031)
  (:SETHOTKEY                    #x0032)
  (:GETHOTKEY                    #x0033)
  (:QUERYDRAGICON                #x0037)
  (:COMPAREITEM                  #x0039)
  (:GETOBJECT                    #x003D)
  (:COMPACTING                   #x0041)
  (:COMMNOTIFY                   #x0044) ;no longer suported
  (:WINDOWPOSCHANGING            #x0046)
  (:WINDOWPOSCHANGED             #x0047)
  (:POWER                        #x0048)
  (:COPYDATA                     #x004A)
  (:CANCELJOURNAL                #x004B)
  (:NOTIFY                       #x004E)
  (:INPUTLANGCHANGEREQUEST       #x0050)
  (:INPUTLANGCHANGE              #x0051)
  (:TCARD                        #x0052)
  (:HELP                         #x0053)
  (:USERCHANGED                  #x0054)
  (:NOTIFYFORMAT                 #x0055)
  (:CONTEXTMENU                  #x007B)
  (:STYLECHANGING                #x007C)
  (:STYLECHANGED                 #x007D)
  (:DISPLAYCHANGE                #x007E)
  (:GETICON                      #x007F)
  (:SETICON                      #x0080)
  (:NCCREATE                     #x0081)
  (:NCDESTROY                    #x0082)
  (:NCCALCSIZE                   #x0083)
  (:NCHITTEST                    #x0084)
  (:NCPAINT                      #x0085)
  (:NCACTIVATE                   #x0086)
  (:GETDLGCODE                   #x0087)
  (:SYNCPAINT                    #x0088)
  (:NCMOUSEMOVE                  #x00A0)
  (:NCLBUTTONDOWN                #x00A1)
  (:NCLBUTTONUP                  #x00A2)
  (:NCLBUTTONDBLCLK              #x00A3)
  (:NCRBUTTONDOWN                #x00A4)
  (:NCRBUTTONUP                  #x00A5)
  (:NCRBUTTONDBLCLK              #x00A6)
  (:NCMBUTTONDOWN                #x00A7)
  (:NCMBUTTONUP                  #x00A8)
  (:NCMBUTTONDBLCLK              #x00A9)
  (:NCXBUTTONDOWN                #x00AB)
  (:NCXBUTTONUP                  #x00AC)
  (:NCXBUTTONDBLCLK              #x00AD)
  (:INPUT_DEVICE_CHANGE          #x00FE)
  (:INPUT                        #x00FF)
  (:KEYFIRST                     #x0100)
  (:KEYDOWN                      #x0100)
  (:KEYUP                        #x0101)
  (:CHAR                         #x0102)
  (:DEADCHAR                     #x0103)
  (:SYSKEYDOWN                   #x0104)
  (:SYSKEYUP                     #x0105)
  (:SYSCHAR                      #x0106)
  (:SYSDEADCHAR                  #x0107)
  (:UNICHAR                      #x0109)
  (:KEYLAST                      #x0109)
					;  (:KEYLAST                      #x0108)
  (:IME_STARTCOMPOSITION         #x010D)
  (:IME_ENDCOMPOSITION           #x010E)
  (:IME_COMPOSITION              #x010F)
  (:IME_KEYLAST                  #x010F)
  (:INITDIALOG                   #x0110)
  (:COMMAND                      #x0111)
  (:SYSCOMMAND                   #x0112)
  (:TIMER                        #x0113)
  (:HSCROLL                      #x0114)
  (:VSCROLL                      #x0115)
  (:INITMENU                     #x0116)
  (:INITMENUPOPUP                #x0117)
  (:GESTURE                      #x0119)
  (:GESTURENOTIFY                #x011A)
  (:MENUSELECT                   #x011F)
  (:MENUCHAR                     #x0120)
  (:ENTERIDLE                    #x0121)
  (:MENURBUTTONUP                #x0122)
  (:MENUDRAG                     #x0123)
  (:MENUGETOBJECT                #x0124)
  (:UNINITMENUPOPUP              #x0125)
  (:MENUCOMMAND                  #x0126)
  (:CHANGEUISTATE                #x0127)
  (:UPDATEUISTATE                #x0128)
  (:QUERYUISTATE                 #x0129)
  (:CTLCOLORMSGBOX               #x0132)
  (:CTLCOLOREDIT                 #x0133)
  (:CTLCOLORLISTBOX              #x0134)
  (:CTLCOLORBTN                  #x0135)
  (:CTLCOLORDLG                  #x0136)
  (:CTLCOLORSCROLLBAR            #x0137)
  (:CTLCOLORSTATIC               #x0138)
  (:MOUSEFIRST                   #x0200)
  (:MOUSEMOVE                    #x0200)
  (:LBUTTONDOWN                  #x0201)
  (:LBUTTONUP                    #x0202)
  (:LBUTTONDBLCLK                #x0203)
  (:RBUTTONDOWN                  #x0204)
  (:RBUTTONUP                    #x0205)
  (:RBUTTONDBLCLK                #x0206)
  (:MBUTTONDOWN                  #x0207)
  (:MBUTTONUP                    #x0208)
  (:MBUTTONDBLCLK                #x0209)
  (:MOUSEWHEEL                   #x020A)
  (:XBUTTONDOWN                  #x020B)
  (:XBUTTONUP                    #x020C)
  (:XBUTTONDBLCLK                #x020D)
  (:MOUSEHWHEEL                  #x020E)
  (:MOUSELAST                    #x020E)
					;  (:MOUSELAST                    #x020D)
					;  (:MOUSELAST                    #x020A)
					;  (:MOUSELAST                    #x0209)
  (:PARENTNOTIFY                 #x0210)
  (:ENTERMENULOOP                #x0211)
  (:EXITMENULOOP                 #x0212)
  (:NEXTMENU                     #x0213)
  (:SIZING                       #x0214)
  (:CAPTURECHANGED               #x0215)
  (:MOVING                       #x0216)
  (:POWERBROADCAST               #x0218)
  (:DEVICECHANGE                 #x0219)
  (:MDICREATE                    #x0220)
  (:MDIDESTROY                   #x0221)
  (:MDIACTIVATE                  #x0222)
  (:MDIRESTORE                   #x0223)
  (:MDINEXT                      #x0224)
  (:MDIMAXIMIZE                  #x0225)
  (:MDITILE                      #x0226)
  (:MDICASCADE                   #x0227)
  (:MDIICONARRANGE               #x0228)
  (:MDIGETACTIVE                 #x0229)
  (:MDISETMENU                   #x0230)
  (:ENTERSIZEMOVE                #x0231)
  (:EXITSIZEMOVE                 #x0232)
  (:DROPFILES                    #x0233)
  (:MDIREFRESHMENU               #x0234)
  (:POINTERDEVICECHANGE          #x238)
  (:POINTERDEVICEINRANGE         #x239)
  (:POINTERDEVICEOUTOFRANGE      #x23A)
  (:TOUCH                        #x0240)
  (:NCPOINTERUPDATE              #x0241)
  (:NCPOINTERDOWN                #x0242)
  (:NCPOINTERUP                  #x0243)
  (:POINTERUPDATE                #x0245)
  (:POINTERDOWN                  #x0246)
  (:POINTERUP                    #x0247)
  (:POINTERENTER                 #x0249)
  (:POINTERLEAVE                 #x024A)
  (:POINTERACTIVATE              #x024B)
  (:POINTERCAPTURECHANGED        #x024C)
  (:TOUCHHITTESTING              #x024D)
  (:POINTERWHEEL                 #x024E)
  (:POINTERHWHEEL                #x024F)
  (:IME_SETCONTEXT               #x0281)
  (:IME_NOTIFY                   #x0282)
  (:IME_CONTROL                  #x0283)
  (:IME_COMPOSITIONFULL          #x0284)
  (:IME_SELECT                   #x0285)
  (:IME_CHAR                     #x0286)
  (:IME_REQUEST                  #x0288)
  (:IME_KEYDOWN                  #x0290)
  (:IME_KEYUP                    #x0291)
  (:MOUSEHOVER                   #x02A1)
  (:MOUSELEAVE                   #x02A3)
  (:NCMOUSEHOVER                 #x02A0)
  (:NCMOUSELEAVE                 #x02A2)
  (:WTSSESSION_CHANGE            #x02B1)
  (:TABLET_FIRST                 #x02c0)
  (:TABLET_LAST                  #x02df)
  (:DPICHANGED                   #x02E0)
  (:CUT                          #x0300)
  (:COPY                         #x0301)
  (:PASTE                        #x0302)
  (:CLEAR                        #x0303)
  (:UNDO                         #x0304)
  (:RENDERFORMAT                 #x0305)
  (:RENDERALLFORMATS             #x0306)
  (:DESTROYCLIPBOARD             #x0307)
  (:DRAWCLIPBOARD                #x0308)
  (:PAINTCLIPBOARD               #x0309)
  (:VSCROLLCLIPBOARD             #x030A)
  (:SIZECLIPBOARD                #x030B)
  (:ASKCBFORMATNAME              #x030C)
  (:CHANGECBCHAIN                #x030D)
  (:HSCROLLCLIPBOARD             #x030E)
  (:QUERYNEWPALETTE              #x030F)
  (:PALETTEISCHANGING            #x0310)
  (:PALETTECHANGED               #x0311)
  (:HOTKEY                       #x0312)
  (:PRINT                        #x0317)
  (:PRINTCLIENT                  #x0318)
  (:APPCOMMAND                   #x0319)
  (:THEMECHANGED                 #x031A)
  (:CLIPBOARDUPDATE              #x031D)
  (:DWMCOMPOSITIONCHANGED        #x031E)
  (:DWMNCRENDERINGCHANGED        #x031F)
  (:DWMCOLORIZATIONCOLORCHANGED  #x0320)
  (:DWMWINDOWMAXIMIZEDCHANGE     #x0321)
  (:DWMSENDICONICTHUMBNAIL           #x0323)
  (:DWMSENDICONICLIVEPREVIEWBITMAP   #x0326)
  (:GETTITLEBARINFOEX            #x033F)
  (:HANDHELDFIRST                #x0358)
  (:HANDHELDLAST                 #x035F)
  (:AFXFIRST                     #x0360)
  (:AFXLAST                      #x037F)
  (:PENWINFIRST                  #x0380)
  (:PENWINLAST                   #x038F)
  (:APP                          #x8000)
  (:USER                         #x0400)
  )

(defcenum (GA_ENUM :unsigned-int)
  (:PARENT 1);Retrieves the parent window. This does not include the owner, as it does with the GetParent function.
  (:ROOT 2);Retrieves the root window by walking the chain of parent windows.
  (:ROOTOWNER 3);Retrieves the owned root window by walking the chain of parent and owner windows returned by GetParent.
  )

;;; 
(defcstruct RECT
  (left :long)
  (top :long)
  (right :long)
  (bottom :long))

(defcstruct PAINTSTRUCT
  (hdc  HDC)
  (fErase :boolean)
  (rcPaint (:struct RECT))
  (fRestore :boolean)
  (fIncUpdate :boolean)
  (rgbReserved C_BYTE :count 32))

(defcstruct POINTL
  (x :long)
  (y :long))

(defcstruct DM_DISPLAY
  (dmPosition (:struct POINTL))
  (dmDisplayOrientation  DWORD)
  (dmDisplayFixedOutput  DWORD))

(defcstruct DM_PRINTER
  (dmOrientation :short)
  (dmPaperSize :short)
  (dmPaperLength :short)
  (dmPaperWidth :short)
  (dmScale :short)
  (dmCopies :short)
  (dmDefaultSource :short)
  (dmPrintQuality :short))

(defcunion DM_DEVICE
  (dmPrinter (:struct DM_PRINTER))
  (dmDisplay (:struct DM_DISPLAY)))

(defcunion DM_MISC
  (dmDisplayFlags DWORD)
  (dmNup DWORD))

(defcstruct DEVMODE 
  (dmDeviceName WORD :count 32)		;wchar_t[32]
  (dmSpecVersion  WORD)
  (dmDriverVersion  WORD)
  (dmSize  WORD) 
  (dmDriverExtra  WORD) 
  (dmFields DWORD) 
  (dmDevice (:union DM_DEVICE)) 
  (dmColor :short)
  (dmDuplex :short)
  (dmYResolution :short)
  (dmTTOption :short)
  (dmCollate :short)
  (dmFormName WORD :count 32)		;wchar_t[32]
  (dmLogPixels  WORD)
  (dmBitsPerPel DWORD)
  (dmPelsWidth DWORD)
  (dmPelsHeight DWORD)
  (dmMisc (:union DM_MISC))
  (dmDisplayFrequency DWORD)
  ;; (WINVER >= 0x0400)
  (dmICMMethod DWORD)
  (dmICMIntent DWORD)
  (dmMediaType DWORD)
  (dmDitherType DWORD)
  (dmReserved1 DWORD)
  (dmReserved2 DWORD)
  ;; (WINVER >= 0x0500) || (_WIN32_WINNT >= 0x0400)
  (dmPanningWidth DWORD)
  (dmPanningHeight DWORD))
