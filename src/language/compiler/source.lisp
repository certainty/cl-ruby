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

(defun from-file (file-path)
  "Creates a source origin from a file."
  (make-instance 'from-file :file-path file-path))

(defclass from-string (source-origin)
  ((content
     :initarg :content
     :initform (a:required-argument "content")
     :type string)
   (context
     :initarg :context
     :initform (a:required-argument "context")
     :type string))
  (:documentation "A source origin that represents a string."))

(defmethod print-object ((origin from-string) stream)
  (print-unreadable-object (origin stream :type t)
    (format stream "~A" (slot-value origin 'context))))

(defun from-string (string &optional (context ""))
  "Creates a source origin from a string."
  (make-instance 'from-string :content string :context context))

(defclass source-code ()
  ((origin
     :initarg :origin
     :initform (a:required-argument "origin")
     :type source-origin)
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

(defmethod open-source-code ((origin from-file))
  "Creates a source code object."
  (with-slots (file-path) origin
    (let ((stream (open file-path :direction :input)))
      (make-instance 'source-code :origin origin :stream stream))))

(defmethod open-source-code ((origin from-string))
  "Creates a source code object."
  (with-slots (content) origin
    (let ((stream (make-string-input-stream content)))
      (make-instance 'source-code :origin origin :stream stream))))

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
