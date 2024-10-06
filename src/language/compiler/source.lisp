(in-package :cl-ruby.source)

(s:defconstructor source-position
  (line (integer 1 *))
  (column (integer 1 *))
  (offset (integer 0 *)))

(defun format-position (position &optional stream)
  "Prints the source position to the standard output."
  (with-slots (line column offset) position
    (format stream "offset ~D line ~D column ~D"
      offset line column)))

(deftype origin-designator ()
  "A designator for the origin of a source code object."
  `(or string pathname))

(defclass source-code ()
  ((origin
     :initarg :origin
     :initform (a:required-argument "origin")
     :type origin-designator)
    (stream
      :reader source-code-stream
      :initarg :stream
      :initform (a::required-argument "stream")
      :type stream))
  (:documentation "A source code object represents a piece of source code."))

(defmethod print-object ((source-code source-code) stream)
 (print-unreadable-object (source-code stream :type t)
   (format stream "Origin: ~A" (slot-value source-code 'origin))))

(defgeneric open-source-code (origin)
  (:documentation "Creates a source code object."))

(defmethod open-source-code ((origin pathname))
  "Creates a source code object."
  (make-instance 'source-code :origin origin :stream (open origin :direction :input)))

(defmethod open-source-code ((origin string))
  "Creates a source code object."
  (make-instance 'source-code :origin "string" :stream (make-string-input-stream origin)))

(defun close-source-code (source-code)
  "Closes the source code object."
  (with-slots (stream) source-code
    (close stream)))

(defmacro with-source-code ((source-code origin-expr) &body body)
  "Executes the body with the source code object."
  `(let ((,source-code (open-source-code ,origin-expr)))
     (unwind-protect
       (progn ,@body)
       (close-source-code ,source-code))))

(defun eofp (source-code)
  "Returns T if the end of the source code has been reached."
  (with-slots (stream) source-code
    (eq :eof (read-char stream nil :eof))))
