(in-package #:w32api.util)

;;; Union type of bitfield-types
(defun bitfield-union-value (symbols bitfield-types)
  (reduce
   (lambda (value bitfield-type)
     (logior value
	     (foreign-bitfield-value bitfield-type
				     (intersection symbols (foreign-bitfield-symbol-list bitfield-type)))))
   bitfield-types
   :initial-value 0))

(defun bitfield-union-symbols (value bitfield-types)
  (mapcan (lambda (bitfield-type)
	    (set-difference
	     (foreign-bitfield-symbols bitfield-type value)
	     (foreign-bitfield-symbols bitfield-type 0)))
	  bitfield-types))

(define-foreign-type bitfield-union-type ()
  ((bitfield-types :reader bitfield-types :initarg :bitfield-types)))

(define-parse-method w32api.util::bitfield-union (base-type &rest bitfield-types)
  (make-instance 'bitfield-union-type :actual-type base-type :bitfield-types bitfield-types)
  )

(defmethod translate-to-foreign (symbols (type bitfield-union-type))
  (bitfield-union-value symbols (bitfield-types type)))

(defmethod translate-from-foreign (value (type bitfield-union-type))
  (bitfield-union-symbols value (bitfield-types type)))

(define-foreign-library user32
  (:win32 "user32.dll"))

;; (use-foreign-library user32)

;; (defbitfield (a :uint) (:a 1))

;; (defcfun "CreateWindowExA" :pointer
;;   (dwExStyle     :uint)
;;   (lpClassName   :string)
;;   (lpWindowName   :string)
;;   (dwStyle     (bitfield-union :uint a))
;;   (x :int)
;;   (y :int)
;;   (nWidth       :int)
;;   (nHeight       :int)
;;   (hWndParent      :pointer)
;;   (hMenu     :pointer)
;;   (hInstance :pointer)
;;   (lpParam    (:pointer :void)))
