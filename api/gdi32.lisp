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
	   FrameRect))

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
