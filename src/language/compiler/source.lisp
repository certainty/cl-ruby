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

(defclass source-origin () ()
  (:documentation "A source origin represents the origin of a source code."))

(defclass from-file (source-origin)
  ((file-path
     :initarg :file-path
     :initform (alexandria:required-argument "file-path")
     :type pathname))
  (:documentation "A source origin that represents a file."))

(defmethod print-object ((origin from-file) stream)
  (print-unreadable-object (origin stream :type t)
    (format stream "~A" (slot-value origin 'file-path))))

(defclass from-string (source-origin)
  ((context
     :initarg :context
     :initform (a:required-argument "context")
     :type string))
  (:documentation "A source origin that represents a string."))

(defmethod print-object ((origin from-string) stream)
  (print-unreadable-object (origin stream :type t)
    (format stream "~A" (slot-value origin 'code))))

(defclass source-code ()
  ((origin
     :initarg :origin
     :initform (a:required-argument "origin")
     :type source-origin)
    (stream
      :initarg :stream
      :initform (a::required-argument "stream")
      :type stream))
  (:documentation "A source code object represents a piece of source code."))

(defmethod print-object ((source-code source-code) stream)
 (print-unreadable-object (source-code stream :type t)
   (format stream "Origin: ~A" (slot-value source-code 'origin))))

(defgeneric with-source-code (origin code &rest args)
  (:documentation "Creates a source code object."))

(defmethod with-source-code ((origin pathname) callback &rest args)
  (with-open-file (stream origin :direction :input)
    (funcall callback
      (make-instance 'source-code :origin (apply #'make-instance 'from-file :file-path origin args) :stream stream))))

(defmethod with-source-code ((origin string) callback &rest args)
  (with-input-from-string (stream origin)
    (funcall callback
      (make-instance 'source-code :origin (apply #'make-instance 'from-string args) :stream stream))))
