(in-package :cl-ruby.lexer)

(define-condition lexer-error (error)
  ((message
     :reader lexer-error-message
     :initarg :message)
   (span
     :reader lexer-error-position
     :initarg :position))
  (:report (lambda (condition stream)
             (format stream "Lexer error at ~a:  ~a"
               (lexer-error-position condition)
               (lexer-error-message condition)))))

(define-condition invalid-token (lexer-error) ())

(defparameter *default-buffer-size* 4096)

(defclass lexer ()
  ((input
     :reader lexer-input
     :initarg :input
     :type stream 
     :documentation "The input that we are scanning.")
    (buffer
      :initarg :buffer
      :initform (a:required-argument "buffer")
      :type (simple-array character)
      :documentation "The internal read buffer for the scanner")
    (offset
      :initform 0
      :type a:array-index
      :documentation "The offset of the current token in the input")
    (line
      :initform 1
      :type a:positive-fixnum
      :documentation "The current line number")
    (column
      :initform 1
      :type a:positive-fixnum
      :documentation "The current column number")
    (base
      :initform 0
      :type a:array-index
      :documentation "The base position of the current token")
    (current
      :initform 0
      :type a:array-index
      :documentation "The current position in the buffer")
    (buffer-water-mark
      :initform 0
      :type a:array-index
      :documentation "The point to which the buffer has been filled. This is normally the size of the buffer, unless we are at the end of the input.")
    (eof
      :initform nil
      :type boolean))
  (:documentation "The lexer is responsible for scanning the input string and returning tokens.
   It keeps track of the current position in the input string, as well as the current line and column number.
   The lexer is also responsible for handling errors, such as illegal tokens."))

(defun make-lexer (stream &key (buffer-size *default-buffer-size*))
  "Creates a new lexer for the given input string."
  (let ((buffer (make-array buffer-size :element-type 'character :adjustable t)))
    (make-instance 'lexer :input stream :buffer buffer)))

(defmethod print-object ((lexer lexer) stream)
  (print-unreadable-object (lexer stream :type t :identity t)
    (with-slots (base current buffer-water-mark eof buffer) lexer
      (format stream "base: ~a, current: ~a, buffer-water-mark: ~a, eof: ~a" base current buffer-water-mark eof))))

(defun scan-token (lexer &key (mode :ruby))
  "Scans the next token from input or returns nil if we have reached the end of the input."
  (declare (ignore mode))
  (unless (at-end-p lexer)
    (let ((c (advance lexer)))
      (s:select c
        (#\( (accept lexer @lparen))
        (#\) (accept lexer @rparen))
        (#\[ (accept lexer @lbracket))
        (#\] (accept lexer @rbracket))
        (#\{ (accept lexer @lbrace))
        (#\} (accept lexer @rbrace))
        (#\; (accept lexer @semicolon))
        (#\, (accept lexer @comma))
        (t (accept lexer @illegal))))))
  
(defun matches (state chr)
  (unless (at-end-p state)
    (when (char= (peek state) chr)
      (advance state)
      t)))

(defun advance* (state n)
  "Advance `n' times in a loop and return the last character."
  (loop :repeat n
    :for c = (advance state)
    :while c
    :finally (return c)))

(defun advance (state)
  "Advance reads the character at the current position, advances the internal cursor by one, and returns the character.
  Example:

   'abcdt'
     ^-- cursor

   (advance state) => #\b

   'abcdt'
      ^-- cursor
 " 
  (with-slots (current line column offset) state
    (a:when-let ((c (peek state)))
      (when (char= c #\Newline)
        (incf line)
        (setf column 0))
      (incf current)
      (incf column)
      (incf offset)
      c)))

(defun peek (state &optional (offset 0))
  "Peek returns the current character, and does not advance the internal cursor."
  (unless (at-end-p state)
	  (with-slots (buffer base current buffer-water-mark) state
      (when (>= current buffer-water-mark)
        (refill-buffer state))
      (unless (>= (+ offset current) buffer-water-mark)
        (aref buffer (+ offset current))))))

(defun make-position (lexer)
  "Creates a new position object for the current position in the input."
  (with-slots (offset line column) lexer
    (source:source-position line column offset)))

(defun refill-buffer (state)
  "Refills the internal buffer with fresh data from the underlying input.
  This also resets the internal values for `base', `current' and `buffer-water-mark'. "
  (with-slots (eof buffer input buffer-water-mark base current) state
    (unless eof 
	    (let ((lexeme (current-lexeme state))
            (buffer-size (length buffer)))
        ;; there might be rare cases where the lexeme is larger than the buffer
        ;; in that case we need to adjust the buffer size
        (when (>= (length lexeme) buffer-size)
          (adjust-array buffer (+ buffer-size (floor buffer-size 2))))
        (setf (subseq buffer 0 (length lexeme)) lexeme)
        (setf buffer-water-mark (read-sequence buffer input :start (length lexeme)))
	      (setf
          eof (< buffer-water-mark buffer-size)
          base 0
          current (length lexeme))))))

(defun at-end-p (state)
  "Returns true if we have reached the end of the input."
  (with-slots (buffer-water-mark current eof) state
    (and
      eof
      (>= current buffer-water-mark))))

(defun current-lexeme (state)
  "Returns the current lexeme, which is the part of the input that we have scanned so far."
  (with-slots (buffer base current) state
    (subseq buffer base current)))

(defun accept (state token-class &key (include-lexeme nil))
  "Accepts the current token and returns it."
	(prog1 (token token-class (and include-lexeme (current-lexeme state)) (make-position state))
    (with-slots (base current) state
      (setf base current))))
