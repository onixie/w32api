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

(defmacro %with-foreign-struct (((var type &optional size-var) &rest slots) &body body)
  `(let ,(when size-var `((,size-var (foreign-type-size ',type))))
     (with-foreign-object (,var ',type)
       (setf ,@(loop for (slot-name slot-val) in slots
		  append
		    (destructuring-bind ((slot-name &optional slot-var) &optional slot-val)
			(list (ensure-list slot-name) slot-val)
		      (declare (ignore slot-var))
		      (when slot-val
			`((foreign-slot-value ,var ',type ,slot-name) ,slot-val)))))
       (symbol-macrolet
	   ,(remove-if #'null
		       (loop for (slot-name slot-val) in slots
			  collect
			    (destructuring-bind ((slot-name &optional slot-var) &optional slot-val)
				(list (ensure-list slot-name) slot-val)
			      (declare (ignore slot-val))
			      `(,(if slot-var slot-var
				     (intern (format nil "~a.~a" var slot-name)))
				 (foreign-slot-value ,var ',type ,slot-name)))))
	 ,@body))))

(defmacro with-foreign-struct (((var type &optional size-var) &rest slots) &body body)
  `(%with-foreign-struct ((,var (:struct ,type) ,size-var) ,@slots) ,@body))

(defmacro with-foreign-union (((var type &optional size-var) &rest slots) &body body)
  `(%with-foreign-struct ((,var (:union ,type) ,size-var) ,@slots) ,@body))
