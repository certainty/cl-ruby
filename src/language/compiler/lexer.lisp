(in-package :cl-ruby.lexer)

;;; https://github.com/ruby/ruby/blob/master/ext/ripper/lib/ripper/lexer.rb
(s:defunion token-class
  @eof
  @illegal
  @semicolon
  @space
  @newline
  @identifier
  )

(defclass token ()
  ((class
     :reader token-class
     :initarg :class
     :initform (error "no type given")
     :type token-class
     :documentation "The type of the token, which is a member of `token-class'")
    (lexeme
      :reader token-lexeme
      :initarg :lexeme
      :initform (error "no lexeme given")
      :type string
      :documentation "The actual string that was matched by the scanner.")
    (value
      :reader token-value
      :initarg :value
      :initform nil
      :type (or null t)
      :documentation
      "The value of the token. This is used mostly for literals, which we can evaluate at compile time to lisp values.
    These are not necessarily equivalent to the runtime values we will eventually get.")
    (position
      :reader token-position
      :initarg :position
      :initform nil
      :type (or null source:source-position)))
  (:documentation "A token is a single unit of input. It is defined by the `class' and it's `lexeme'.
   The `class' represents the type of the token.
   The `lexeme' represents the actual string that was matched by the scanner.
   For many tokens, the `lexeme' does not contain any useful information.
   For example, the `lexeme' for the `PLUS' token is the string \"+\".
   However, for some tokens, the `lexeme' is very important.
   For example, the `lexeme' for an `identifier' token is the actual identifier.
   "))

(defmethod print-object ((token token) stream)
  (with-slots (class lexeme value) token
    (print-unreadable-object (token stream :type t :identity t)
      (format stream "class: ~a lexeme: ~a value: ~S" class lexeme value))))

(defun synthetic-eof ()
  "Returns a synthetic EOF token. This is used to mark the end of the input."
  (make-instance 'token
    :class @eof
    :lexeme ""))

(define-condition lexer-error (error)
  ((message
     :reader lexer-error-message
     :initarg :message)
   (span
     :reader lexer-error-position
     :initarg :position))
  (:report (lambda (condition stream)
             (format stream "Illegal token at ~a" (lexer-error-position condition)))))

(define-condition invalid-token (lexer-error) ())

(defclass lexer ()
  ((input
     :reader lexer-input
     :initarg :input
     :type source:source-code
     :documentation "The input string that we are scanning.")
    (error-count
     :initform 0
     :type (integer 0 *)
     :documentation "The number of errors that have occurred while scanning the input string.")
    (base
      :initarg :base
      :type source:cursor
      :documentation "The current base cursor into the input string")
    (look-ahead
      :initarg :look-ahead
      :type source:cursor
      :documentation "The current look-ahead cursor into the input string"))
  (:documentation "The lexer is responsible for scanning the input string and returning tokens.
   It keeps track of the current position in the input string, as well as the current line and column number.
   The lexer is also responsible for handling errors, such as illegal tokens."))

(defun make-lexer (input)
  "Creates a new lexer for the given input string."
  (let ((source (source:make-source-code input)))
    (make-instance 'lexer
      :input source
      :base (source:cursor-from-beginning source)
      :look-ahead (source:cursor-from-beginning source))))

(defmethod print-object ((lexer lexer) stream)
  (with-slots (input base look-ahead) lexer
    (print-unreadable-object (lexer stream :type t :identity t)
      (format stream "input: ~a base: ~a look-ahead: ~a" input base look-ahead))))

(defun lex (input)
  "Scans the input string and returns the values:
    - a vector of tokens
    - a boolean indicating if there were any errors
    - the number of errors that occurred
"
  (let ((lexer (make-lexer input)))
    (with-slots (error-count) lexer
      (values (scan-til-eof lexer) (plusp error-count) error-count))))

(defun scan-til-eof (lexer)
  (loop
    :with tokens = nil
    :for token = (next-token lexer)
    :until (eofp lexer)
    :do (push token tokens)
    :finally (return (coerce (nreverse tokens) 'vector) )))

(defun scan-token (lexer)
  "Scans the next token and returns it. If there is an error, then it will signal an `invalid-token' condition."
  (or (scan-eof lexer)))

(defun scan-eof (lexer)
  "Scans the end of the input and returns the EOF token."
  (if (eofp lexer)
    (accept lexer @eof)))

;;; Scanning functions and combinators
(defun recover (lexer)
  "Advances the scanner to the next statement boundary, which is either a newline or a semicolon."
  (skip-until lexer #'statement-boundary-p)
  lexer)

(defun statement-boundary-p (c)
  (or (char= c #\newline) (char= c #\;)))

(defmacro skip-on-error (&body body)
  "Use this to wrap the scanning process and it will handle scan errors by inserting the special `@invalid' token and continue scanning at the next token"
  `(handler-bind ((invalid-token #'skip-to-next-token))
     ,@body))

(defun skip-to-next-token ()
  "Invoke the restart `skip-to-next-token' to skip to the next token"
  (let ((restart (find-restart 'skip-to-next-token)))
    (when restart
      (invoke-restart restart))))

(defun next-token (lexer)
  "Attempt to scan the next token and return it. This is the main interface to the lexer.
Example:
  ```  
    (defvar lexer (make-lexer \"1 + 2\")
    (next-token lexer)
  ```
"
  (restart-case (scan-token lexer)
    (skip-to-next-token ()
      (incf (slot-value lexer 'error-count))
      (recover lexer)
      (accept lexer @illegal))))

(defun scan-while (lexer predicate)
  "Scans the input string while the predicate is true."
  (with-slots (look-ahead) lexer
    (loop :for c = (source:cursor-value look-ahead)
      :while (and c (funcall predicate c))
      :do (advance lexer))))

(defun skip-until (lexer predicate)
  "Skips the input string until the predicate is true."
  (with-slots (look-ahead) lexer
    (loop :for c = (source:cursor-value look-ahead)
      :while (and c (not (funcall predicate c)))
      :do (advance lexer)
      :finally (skip lexer))))

(defun skip-while (lexer predicate)
  "Skips the input string while the predicate is true."
  (skip-until lexer (complement predicate)))

(defun eofp (lexer)
  (with-slots (base) lexer
    (source:cursor-eofp base)))

(defun advance (lexer &key (by 1))
  "Advances the look-ahead cursor by the given number of characters. Returns the character at the final position.
  If the cursor is already at the end of the input, then it returns nil."
  (with-slots (look-ahead) lexer
    (loop :repeat by :do (source:cursor-advance look-ahead))
    (unless (source:cursor-eofp look-ahead)
      (source:cursor-value look-ahead))))

(defun accept (lexer token-class &key (transform-value #'identity))
  "Accepts the input between the `base' and `look-ahead' cursor as the next token with the provided token-class.
This will advance the `base' cursor to the `look-ahead' cursor and return the token.
"
  (with-slots (base look-ahead) lexer
    (let* ((lexeme (source:cursor-value base :end-cursor look-ahead :eof-is-error-p nil))
            (token (make-instance 'token :class token-class :lexeme lexeme :value (funcall transform-value lexeme) :position (source:cursor-position base))))
      (setf base (source:cursor-clone look-ahead))
      token)))

(defun skip (lexer)
  "Skips the input between the `base' and `look-ahead' cursor."
  (with-slots (base look-ahead) lexer
    (setf base (source:cursor-clone look-ahead))))
