(defpackage #:w32api.gdi32
  (:use #:common-lisp #:cffi #:w32api.type)
  (:export TextOutW
	   MoveToEx
	   LineTo
	   Polyline
	   PolylineTo
	   PolyPolyline
	   PolyBezier
	   PolyBezierTo
	   PolyDraw
	   SetArcDirection
	   GetArcDirection
	   Arc
	   ArcTo
	   AngleArc
	   Ellipse
	   Rectangle
	   RoundRect
	   FillRect
	   Polygon
	   PolyPolygon
	   Pie
	   Chord
	   InvertRect
	   FrameRect
	   SetBkMode
	   GetBkMode
	   GetBkColor
	   SetBkColor
	   GetTextExtentPoint32W
	   GetTextMetricsW
	   GetTextColor
	   SetTextColor
	   SelectObject
	   DeleteObject
	   GetObjectW
	   GetCurrentObject
	   CreatePen
	   ExtCreatePen
	   CreateSolidBrush
	   CreateHatchBrush
	   CreatePatternBrush
	   CreateBrushIndirect
	   CreateFontW
	   CreateFontIndirectW
	   EnumFontFamiliesExW
	   GetStockObject
	   GetPixel
	   SetPixel
	   GetDCPenColor
	   SetDCPenColor
	   GetDCBrushColor
	   SetDCBrushColor
	   BeginPath
	   EndPath
	   AbortPath
	   CloseFigure
	   FillPath
	   StrokeAndFillPath
	   StrokePath
	   WidenPath
	   FlattenPath
	   PathToRegion
	   SaveDC
	   RestoreDC
	   CreateCompatibleDC
	   CreateCompatibleBitmap
	   DeleteDC
	   BitBlt))

(in-package #:w32api.gdi32)

(define-foreign-library gdi32
  (:win32 "gdi32.dll"))

(use-foreign-library "gdi32")

(defcfun "TextOutW" :boolean
  (hdc HDC)
  (nXStart :int)
  (nYStart :int)
  (lpString :string)
  (cchString :int))

(defcfun "MoveToEx" :boolean
  (hdc HDC)
  (X :int)
  (Y :int)
  (lpPoint (:pointer (:struct POINT))))

(defcfun "LineTo" :boolean
  (hdc HDC)
  (nXEnd :int)
  (nYEnd :int))

(defcfun "Polyline" :boolean
  (hdc HDC)
  (lppt (:pointer (:struct POINT)))
  (cCount DWORD))

(defcfun "PolylineTo" :boolean
  (hdc HDC)
  (lppt (:pointer (:struct POINT)))
  (cCount DWORD))

(defcfun "PolyPolyline" :boolean
  (hdc HDC)
  (lppt (:pointer (:struct POINT)))
  (lpdwPolyPoints (:pointer DWORD))
  (cCount DWORD))

(defcfun "PolyBezier" :boolean
  (hdc HDC)
  (lppt (:pointer (:struct POINT)))
  (cCount DWORD))

(defcfun "PolyBezierTo" :boolean
  (hdc HDC)
  (lppt (:pointer (:struct POINT)))
  (cCount DWORD))

(defparameter +PT_CLOSEFIGURE+      #x01)
(defparameter +PT_LINETO+           #x02)
(defparameter +PT_BEZIERTO+         #x04)
(defparameter +PT_MOVETO+           #x06)

(defcfun "PolyDraw" :boolean
  (hdc HDC)
  (lppt (:pointer (:struct POINT)))
  (lpbTypes (:pointer C_BYTE))
  (cCount DWORD))

(defcenum (AD_ENUM :int)
  (:AD_COUNTERCLOCKWISE 1)
  (:AD_CLOCKWISE        2))

(defcfun "SetArcDirection" :int
  (hdc HDC)
  (ArcDirection AD_ENUM))

(defcfun "GetArcDirection" :int
  (hdc HDC))

(defcfun "Arc" :boolean
  (hdc HDC)
  (nLeftRect :int)
  (nTopRect :int)
  (nRightRect :int)
  (nBottomRect :int)
  (nXStartArc :int)
  (nYStartArc :int)
  (nXEndArc :int)
  (nYEndArc :int))

(defcfun "ArcTo" :boolean
  (hdc HDC)
  (nLeftRect :int)
  (nTopRect :int)
  (nRightRect :int)
  (nBottomRect :int)
  (nXRadial1 :int)
  (nYRadial1 :int)
  (nXRadial2 :int)
  (nYRadial2 :int)
  )

(defcfun "AngleArc" :boolean
  (hdc   HDC)
  (X   :int)
  (Y   :int)
  (dwRadius DWORD)
  (eStartAngle :float)
  (eSweepAngle :float))

(defcfun "Ellipse" :boolean
  (hdc HDC)
  (nLeftRect :int)
  (nTopRect :int)
  (nRightRect :int)
  (nBottomRect :int))

(defcfun "Rectangle" :boolean
  (hdc HDC)
  (nLeftRect :int)
  (nTopRect :int)
  (nRightRect :int)
  (nBottomRect :int))

(defcfun "RoundRect" :boolean
  (hdc HDC)
  (nLeftRect :int)
  (nTopRect :int)
  (nRightRect :int)
  (nBottomRect :int)
  (nWidth :int)
  (nHeight :int)
  )

(defcfun "FillRect" :int
  (hDC    HDC)
  (lprc   (:pointer (:struct RECT)))
  (hbr HBRUSH))

(defcfun "Polygon" :boolean
  (hdc HDC)
  (lpPoints (:pointer (:struct POINT)))
  (nCount :int))

(defcfun "PolyPolygon" :boolean
  (hdc HDC)
  (lpPoints (:pointer (:struct POINT)))
  (lpPolyCounts (:pointer :int))
  (nCount :int))

(defcfun "Pie" :boolean
  (hdc HDC)
  (nLeftRect :int)
  (nTopRect :int)
  (nRightRect :int)
  (nBottomRect :int)
  (nXRadial1 :int)
  (nYRadial1 :int)
  (nXRadial2 :int)
  (nYRadial2 :int)
  )

(defcfun "Chord" :boolean
  (hdc HDC)
  (nLeftRect :int)
  (nTopRect :int)
  (nRightRect :int)
  (nBottomRect :int)
  (nXRadial1 :int)
  (nYRadial1 :int)
  (nXRadial2 :int)
  (nYRadial2 :int)
  )

(defcfun "InvertRect" :boolean
  (hDC  HDC)
  (lprc (:pointer (:struct RECT))))

(defcfun "FrameRect" :boolean
  (hDC  HDC)
  (lprc (:pointer (:struct RECT)))
  (hbr HBRUSH))

(defcfun "SetBkMode" BKM_ENUM
  (hdc HDC)
  (iBkMode BKM_ENUM))

(defcfun "GetBkMode" BKM_ENUM
  (hdc HDC))

(defcfun "GetBkColor" COLORREF
  (hdc HDC))

(defcfun "SetBkColor" COLORREF
  (hdc HDC)
  (crColor COLORREF))

(defcfun "GetTextMetricsW" :boolean
  (hdc HDC)
  (lptm (:pointer (:struct TEXTMETRICW)))) 

(defcfun "GetTextColor" COLORREF
  (hdc HDC))

(defcfun "SetTextColor" COLORREF
  (hdc HDC)
  (crColor COLORREF))

(defcfun "SelectObject" HGDIOBJ
  (hdc     HDC)
  (hgdiobj HGDIOBJ))

(defcfun "GetCurrentObject" HGDIOBJ
  (hdc  HDC)
  (uObjectType OBJ_ENUM))

(defcfun "GetObjectW" :int
  (hgdiobj HGDIOBJ)
  (cbBuffer :int)
  (lpvObject :pointer))

(defcfun "DeleteObject" :boolean
  (hObject HGDIOBJ))					;

(defcfun "CreatePen" HPEN
  (fnPenStyle PS_STYLE_ENUM)
  (nWidth     :int)
  (crColor    COLORREF))

(defcfun "CreatePenIndirect" HPEN
  (lplgpn (:pointer (:struct LOGPEN))))

(defcfun "ExtCreatePen" HPEN
  (dwPenStyle DWORD)			;PS_ENUM and PS_FLAG
  (dwWidth    DWORD)
  (lplb (:pointer (:struct LOGBRUSH)))
  (dwStyleCount DWORD)
  (lpStyle (:pointer DWORD)))

(defcfun "CreateSolidBrush" HBRUSH
  (crColor COLORREF))

(defcfun "CreateHatchBrush" HBRUSH
  (fnStyle HS_ENUM)
  (clrref COLORREF))

(defcfun "CreatePatternBrush" HBRUSH
  (hbmp HBITMAP))

(defcfun "CreateBrushIndirect" HBRUSH
  (lplb (:pointer (:struct LOGBRUSH))))

(defcfun "GetTextExtentPoint32W" :boolean
  (hdc HDC)
  (lpString :string)
  (c :int)
  (lpSize (:pointer (:struct SIZE))))

(defcfun "CreateFontW" HFONT
  (nHeight     :int)
  (nWidth      :int)
  (nEscapement :int)
  (nOrientation :int)
  (fnWeight     :int) ;FW_ENUM
  (fdwItalic      :boolean)
  (fdwUnderline   :boolean)
  (fdwStrikeOut   :boolean)
  (fdwCharSet     CHARSET_ENUM)
  (fdwOutputPrecision OUT_PRECIS_ENUM)
  (fdwClipPrecision CLIP_PRECIS_ENUM)
  (fdwQuality QUALITY_ENUM)
  (fdwPitchAndFamily DWORD)		;PITCH_ENUM and FAMILY_ENUM
  (lpszFace :string))

(defcfun "CreateFontIndirectW" HFONT
  (lplf (:pointer (:struct LOGFONTW))))

(defcfun "EnumFontFamiliesExW" :int
  (hdc HDC)
  (lpLogfont (:pointer (:struct LOGFONTW)))
  (lpEnumFontFamExProc :pointer)
  (lParam LPARAM)
  (dwFlags DWORD))

(defcfun "GetStockObject" HGDIOBJ
  (fnObject STKOBJ_ENUM))

(defcfun "SetPixel" COLORREF
  (hdc HDC)
  (X :int)
  (Y :int)
  (crColor COLORREF))

(defcfun "GetPixel" COLORREF
  (hdc HDC)
  (nXPos :int)
  (nYPos :int))					;

(defcfun "GetDCPenColor" COLORREF
  (hdc      HDC))

(defcfun "SetDCPenColor" COLORREF
  (hdc      HDC)
  (crColor COLORREF))

(defcfun "GetDCBrushColor" COLORREF
  (hdc      HDC))

(defcfun "SetDCBrushColor" COLORREF
  (hdc      HDC)
  (crColor COLORREF))

(defcfun "BeginPath" :boolean
  (hdc HDC))

(defcfun "EndPath" :boolean
  (hdc HDC))

(defcfun "AbortPath" :boolean
  (hdc HDC))

(defcfun "CloseFigure" :boolean
  (hdc HDC))

(defcfun "FillPath" :boolean
  (hdc HDC))

(defcfun "StrokeAndFillPath" :boolean
  (hdc HDC))

(defcfun "StrokePath" :boolean
  (hdc HDC))

(defcfun "WidenPath" :boolean
  (hdc HDC))

(defcfun "FlattenPath" :boolean
  (hdc HDC))

(defcfun "PathToRegion" HRGN
  (hdc HDC))

(defcfun "SaveDC" :int
  (hdc HDC))

(defcfun "RestoreDC" :boolean
  (hdc HDC)
  (nSaveDC :int))

(defcfun "CreateCompatibleDC" HDC
  (hdc HDC))

(defcfun "CreateCompatibleBitmap" HBITMAP
  (hdc HDC)
  (nWidth :int)
  (nHeight :int)) 

(defcfun "DeleteDC" :boolean
  (hDC  HDC))

(defcfun "BitBlt" :boolean
  (hdcDest   HDC)
  (nXDest   :int)
  (nYDest   :int)
  (nWidth   :int)
  (nHeight   :int)
  (hdcSrc   HDC)
  (nXSrc   :int)
  (nYSrc   :int)
  (dwRop ROP_ENUM))

