(defpackage #:w32api.msimg32
  (:use #:common-lisp #:cffi #:w32api.type)
  (:export AlphaBlend
	   TransparentBlt))

(in-package #:w32api.msimg32)

(define-foreign-library msimg32
  (:win32 "msimg32.dll"))

(use-foreign-library "msimg32")

(defcfun "AlphaBlend" :boolean
  (hdcDest HDC)
  (xoriginDest :int)
  (yoriginDest :int)
  (wDest :int)
  (hDest :int)
  (hdcSrc HDC)
  (xoriginSrc :int)
  (yoriginSrc :int)
  (wSrc :int)
  (hSrc :int)
  (blendfunction DWORD))

(defcfun "TransparentBlt" :boolean
  (hdcDest   HDC)
  (nXDest   :int)
  (nYDest   :int)
  (nWidth   :int)
  (nHeight   :int)
  (hdcSrc   HDC)
  (nXSrc   :int)
  (nYSrc   :int)
  (crTransparent :uint))
