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
     :reader file-path
     :initarg :file-path
     :initform (error "You must provide a :file-path initarg.")
     :type pathname))
  (:documentation "A source origin that represents a file."))

(defmethod print-object ((origin from-file) stream)
  (print-unreadable-object (origin stream :type t)
    (format stream "~A" (file-path origin))))

(defclass from-string (source-origin)
  ((code
     :reader code
     :initarg :code
     :initform (error "You must provide a :code initarg.")
     :type string))
  (:documentation "A source origin that represents a string."))

(defmethod print-object ((origin from-string) stream)
  (print-unreadable-object (origin stream :type t)
    (format stream "~S" (excerpt (code origin)))))

(defun excerpt (string &key (length 20) (ellipsis "..."))
  (if (< (length string) length)
    string
    (concatenate 'string (subseq string 0 (- length (length ellipsis))) ellipsis)))

(defclass source-code ()
  ((origin
     :reader origin
     :initarg :origin
     :initform (error "You must provide a :origin initarg.")
     :type source-origin)
    (code
      :reader code
      :initarg :code
      :initform (error "You must provide a :code initarg.")
      :type string))
  (:documentation "A convenient representation of source code, which is used by the lexer."))

(defmethod print-object ((source-code source-code) stream)
 (print-unreadable-object (source-code stream :type t)
   (format stream "Origin: ~A Code: ~S"
     (origin source-code)
     (excerpt (code source-code)))))

(defgeneric make-source-code (origin)
  (:documentation "Creates a source code object."))

(defmethod make-source-code ((origin pathname))
  (make-instance 'source-code
    :origin (make-instance 'from-file :file-path origin)
    :code (with-open-file (stream origin :direction :input)
            (let ((code (make-string (file-length stream))))
              (read-sequence code stream)
              code))))

(defmethod make-source-code ((origin string))
  (make-instance 'source-code
    :origin (make-instance 'from-string :code origin)
    :code origin))

(defclass cursor ()
  ((source-code
     :initarg :source-code
     :initform (error "You must provide a :source-code initarg.")
     :type source-code)
    (line
      :reader line
      :initarg :line
      :initform 1
      :type (integer 1 *))
    (column
      :reader column
      :initarg :column
      :initform 1
      :type (integer 1 *))
    (offset 
      :reader offset
      :initarg :offset
      :initform 0
      :type (integer 0 *)))
  (:documentation "A cursor is used to keep track of the current position in a source code."))

(defmethod print-object ((cursor cursor) stream)
  (print-unreadable-object (cursor stream :type t)
    (format stream "Line: ~D Column: ~D Offset: ~D"
      (line cursor)
      (column cursor)
      (offset cursor))))

(defun cursor-from-beginning (source-code)
  "Creates a cursor that is positioned at the beginning of the source code."
  (make-instance 'cursor
    :source-code source-code
    :line 1
    :column 1
    :offset 0))

(defun cursor-eofp (cursor)
  "Returns true if the cursor is at the end of the source code."
  (>= (offset cursor) (length (code (slot-value cursor 'source-code)))))

(defun cursor-advance (cursor)
  "Advances the cursor to the next position in the source code.
   Returns two values: the updated cursor and a boolean that is true if the cursor is at the end of the source code."
  (with-slots (offset line column source-code) cursor
    (when (cursor-eofp cursor)
      (return-from cursor-advance (values cursor t)))
    (let ((buffer (code source-code)))
      (incf column)
      (when (char= (aref buffer offset) #\Newline)
        (incf line)
        (setf column 1))
      (incf offset)
      (values cursor (cursor-eofp cursor)))))

(defun cursor-retreat (cursor)
  "Retreats the cursor to the previous position in the source code.
   Returns two values: the updated cursor and a boolean that is true if the cursor is at the beginning of the source code."
  (with-slots (offset line column source-code) cursor
    (let ((buffer (code source-code)))
      (when (zerop offset)
        (return-from cursor-retreat (values cursor t)))
      (decf offset)
      (decf column)
      (when (char= (aref buffer offset) #\Newline)
        (decf line)
        (setf column 1))
      (values cursor (zerop offset)))))

(defun cursor-value (cursor &key end-cursor eof-is-error-p)
  "Returns the character that the cursor if currently pointing at.
   If `end-cursor' is provided, it must be a cursor that is ahead of `cursor'. In this case, the function will return the substring between the two cursors.
   If `eof-is-error-p' is true, an error will be signaled if any `cursor' is at the end of the source code."

  (with-slots (offset source-code) cursor
    (when (cursor-eofp cursor)
      (if eof-is-error-p 
        (error "Cursor is at the end of the source code.")
        (return-from cursor-value nil)))

    (unless end-cursor
      (return-from cursor-value (aref (code source-code) offset)))

    (unless (eq (slot-value cursor 'source-code) (slot-value end-cursor 'source-code))
      (error "The end cursor is not from the same source code."))

    (with-slots (offset source-code) cursor
      (if (cursor-eofp end-cursor)
        (if eof-is-error-p
          (error "End cursor is at the end of the source code.")
          (return-from cursor-value (subseq (code source-code) offset)))
        (if (> offset (offset end-cursor))
          (error "The end cursor is not ahead of the cursor.")
          (subseq (code source-code) offset (offset end-cursor)))))))

(defun cursor-clone (cursor)
  "Creates a clone of the cursor."
  (make-instance 'cursor
    :source-code (slot-value cursor 'source-code)
    :line (line cursor)
    :column (column cursor)
    :offset (offset cursor)))

(defun cursor-rewind (cursor)
  "Rewinds the cursor to the beginning of the source code."
  (with-slots (line column offset) cursor
    (setf line 1 column 1 offset 0)
    cursor))

(defun cursor-position (cursor)
  "Returns the source position of the cursor."
  (source-position
    (line cursor)
    (column cursor)
    (offset cursor)))
