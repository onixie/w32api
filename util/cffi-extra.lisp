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

(define-parse-method bitfield-union (base-type &rest bitfield-types)
  (make-instance 'bitfield-union-type :actual-type base-type :bitfield-types bitfield-types)
  )

(defmethod translate-to-foreign (symbols (type bitfield-union-type))
  (bitfield-union-value symbols (bitfield-types type)))

(defmethod translate-from-foreign (value (type bitfield-union-type))
  (bitfield-union-symbols value (bitfield-types type)))

;;; Union type of enum-types
(defun enum-union-value (keyword-or-value enum-types)
  (if (keywordp keyword-or-value)
      (loop for enum-type in enum-types
	 do (let ((value (foreign-enum-value enum-type keyword-or-value :errorp nil)))
	      (when value (return value)))
	 finally (return 0))
      keyword-or-value))

(defun enum-union-keyword-or-value (value enum-types)
  (loop for enum-type in enum-types
     do (let ((keyword (foreign-enum-keyword enum-type value :errorp nil)))
	  (when keyword (return keyword)))
     finally (return value)))

(define-foreign-type enum-union-type ()
  ((enum-types :reader enum-types :initarg :enum-types)))

(define-parse-method enum-union (base-type &rest enum-types)
  (make-instance 'enum-union-type :actual-type base-type :enum-types enum-types))

(defmethod translate-to-foreign (keyword-or-value (type enum-union-type))
  (enum-union-value keyword-or-value (enum-types type)))

(defmethod translate-from-foreign (value (type enum-union-type))
  (enum-union-keyword-or-value value (enum-types type)))

;;; dangerous!!!exposure cffi low level
;;; create a temp callback,
;;; notice cffi says that not all cl impl support non-top level defcallback
(defvar *with-callback-lock* (make-recursive-lock))
(defmacro with-callback ((name &rest args) &body body)
  (let ((actual-name (gensym (string name))))
    `(symbol-macrolet ((,name (callback ,actual-name)))
       (with-recursive-lock-held (*with-callback-lock*)
	 (unwind-protect
	      (progn
		(defcallback ,actual-name ,@args)
		,@body)
	   (remhash ',actual-name cffi-sys::*callbacks*))))))
