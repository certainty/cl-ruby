(in-package :cl-ruby.lexer)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *token-class-id-seq*)
  (declaim (type (unsigned-byte 8) *token-class-id-seq*))

  (defvar *token-class-to-name*)
  (defvar *keyword-token-classes*)

  (setf *token-class-id-seq* 0)
  (setf *token-class-to-name* (make-hash-table))
  (setf *keyword-token-classes* (make-hash-table :test 'equal)))

(defmacro deftoken-class (name)
  "Define a token class with the given name. This will define a constant with the given name and a unique id. It will also define a lookup table for the name to class and class to name."
  (let ((id (incf *token-class-id-seq*)))
    `(progn
      (s:defconst ,name ,id)
      (setf (gethash ,id *token-class-to-name*) ',name))))

(defmacro deftoken-class* (&rest names)
  `(progn
     ,@(loop :for name :in names
         :collect `(deftoken-class ,name))))

(defmacro defkeyword-class (name lexeme)
  `(progn
     (deftoken-class ,name)
     (setf (gethash ,lexeme *keyword-token-classes*) ,name)))

(defmacro defkeyword-class* (&rest keywords)
  `(progn
     ,@(loop :for (name lexeme) :on keywords :by #'cddr
         :collect `(defkeyword-class ,name ,(string-downcase lexeme)))))

;;; https://github.com/ruby/ruby/blob/master/ext/ripper/lib/ripper/lexer.rb

;; special
(deftoken-class* @eof @illegal @ignored)

;; whitespace
(deftoken-class* @space @newline)

;; punctuation
(deftoken-class* @semicolon @comma @dot @lbrace @rbrace @lparen @rparen @lbracket @rbracket @colon)

;; operators
(deftoken-class* @op_scope @op_and @op_or @op_not @op_qmark)

;; literals
(deftoken-class* @identifier @constant @number @string @symbol @ivar @cvar @gvar)

;; keywords
(defkeyword-class*
  @kw_def "def"
  @kw_class "class"
  @kw_module "module"
  @kw_if "if"
  @kw_next "next"
  @kw_else "else"
  @kw_elsif "elsif"
  @kw_unless "unless"
  @kw_case "case"
  @kw_when "when"
  @kw_while "while"
  @kw_until "until"
  @kw_for "for"
  @kw_in "in"
  @kw_do "do"
  @kw_begin "begin"
  @kw_rescue "rescue"
  @kw_ensure "ensure"
  @kw_end "end"
  @kw_yield "yield"
  @kw_super "super"
  @kw_self "self"
  @kw_true "true"
  @kw_false "false"
  @kw_nil "nil"
  @kw_alias "alias"
  @kw_undef "undef"
  @kw_defined "defined?")


(s:defconstructor token
  (class (unsigned-byte 8))
  (lexeme (or null string))
  (value (or null t))
  (position (or null source:source-position)))

(defmethod print-object ((token token) stream)
  (with-slots (class lexeme value position) token
    (print-unreadable-object (token stream :type t :identity nil)
      (format stream "class: ~a lexeme: ~a value: ~S position: ~a" (gethash class *token-class-to-name*) lexeme value position))))

(defun token-class= (token rhs-class)
  "Returns true if the token's class is equal to the given class."
  (with-slots (class) token
    (eq class rhs-class)))

(define-condition lexer-error (error)
  ((message
     :reader lexer-error-message
     :initarg :message)
   (span
     :reader lexer-error-position
     :initarg :position))
  (:report (lambda (condition stream)
             (let ((position (lexer-error-position condition)))
               (format stream "Lexer error at ~a:  ~a"
                 (source:format-position position)
                 (lexer-error-message condition))))))

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

(defun tokenize (input)
  "Scans the input string and returns the values:
    - a vector of tokens
    - a boolean indicating if there were any errors
    - the number of errors that occurred
"
  (let ((lexer (make-lexer input)))
    (with-slots (error-count) lexer
      (values (scan-all lexer) (plusp error-count) error-count))))

(defun scan-all (lexer)
  (loop
    :with tokens = nil
    :for token = (next-token lexer)
    :do (unless (token-class= token @eof)
          (push token tokens))
    :until (eofp lexer)
    :finally (return (coerce (nreverse tokens) 'vector))))

(defun scan-token (lexer)
  "Scans the next token and returns it. If there is an error, then it will signal an `invalid-token' condition."
  (with-slots (look-ahead) lexer
    (let ((c (advance lexer)))
      (cond
        ((null c) (accept lexer @eof))
        ((char= c #\newline) (accept lexer @newline))
        ((sb-unicode:whitespace-p c)
          (advance-while lexer #'sb-unicode:whitespace-p)
          (accept lexer @space))
        ;; comments

        ;; punctuation
        ((char= c #\;) (accept lexer @semicolon))
        ((char= c #\,) (accept lexer @comma))
        ((char= c #\.) (accept lexer @dot))
        ((char= c #\{) (accept lexer @lbrace))
        ((char= c #\}) (accept lexer @rbrace))
        ((char= c #\[) (accept lexer @lbracket))
        ((char= c #\]) (accept lexer @rbracket))
        ((char= c #\() (accept lexer @lparen))
        ((char= c #\)) (accept lexer @rparen))

        ((char= c #\:)
          (advance lexer)
          (let ((c (peek lexer)))
            (retreat lexer)
            (when c
              (cond
                ((inter-token-space-p c) (accept lexer @colon))
                ((char= c #\:) (accept lexer @op_scope))
                (t (scan-symbol-literal lexer))))))

        ((char= c #\*) nil)
        ((char= c #\+) nil)
        ((char= c #\-) nil)
        ((char= c #\&) nil)
        ((char= c #\|) nil)
        ((char= c #\~) nil)

        ((char= c #\?)
          (advance lexer)
          (let ((c (peek lexer)))
            (retreat lexer)
            (when c 
              (if (inter-token-space-p c)
                (accept lexer @op_qmark)
                (scan-single-char-string-literal lexer)))))
        
        ;; number literals
        ((digit-char-p c) (scan-number-literal lexer))

        ;; string literals
        ((char= c #\") (scan-string-literal lexer))

        ((char= c #\') (scan-string-literal lexer))

        ;; variables
        ((char= c #\$)
          (scan-variable lexer @gvar))

        ((char= c #\@)
          (a:when-let ((c (peek lexer)))
            (when (char= c #\@)
              (scan-variable lexer @cvar)
              (scan-variable lexer @ivar))))

        ;; heredocs

        ;; percent literals

        ;; identifiers / constants / keywords
        ((sb-unicode:alphabetic-p c)
          (if (sb-unicode:uppercase-p c)
            (scan-constant lexer)
            (scan-identifier-or-keyword lexer)))

        (t (error 'invalid-token :position (source:cursor-position look-ahead) :message "Invalid token"))))))

(defun scan-number-literal (lexer)
  (advance-while lexer #'digit-char-p)
  (accept lexer @number :transform-value #'parse-integer :include-lexeme t))

(defun scan-single-char-string-literal (lexer)
  nil)

(defun scan-string-literal (lexer)
  nil)

(defun scan-symbol-literal (lexer)
  nil)

(defun inter-token-space-p (c)
  (sb-unicode:whitespace-p c))

(defun identifier-char-p (c)
  (or
    (sb-unicode:alphabetic-p c)
    (digit-char-p c)
    (char= c #\_)
    (char= c #\?)))

(defun scan-identifier-or-keyword (lexer)
  (advance-while lexer #'identifier-char-p)
  (let ((lexeme (current-lexeme lexer)))
    (a:if-let ((kw (gethash lexeme *keyword-token-classes*)))
      (accept lexer kw)
      (accept lexer @identifier :include-lexeme t))))

(defun constant-char-p (c)
  (or
    (sb-unicode:alphabetic-p c)
    (digit-char-p c)
    (char= c #\_)))

(defun scan-constant (lexer)
  (advance-while lexer #'constant-char-p)
  (accept lexer @constant :include-lexeme t))

(defun scan-variable (lexer token-class)
  (advance-while lexer #'identifier-char-p)
  (accept lexer token-class :include-lexeme t))

;;; Scanning functions and combinators
(defun recover (lexer)
  "Advances the scanner to the next statement boundary, which is either a newline or a semicolon."
  (advance-until lexer #'token-boundary-p)
  lexer)

(defun token-boundary-p (c)
  (when c
    (or
      (sb-unicode:whitespace-p c)
      (char= c #\;))))

(defmacro skip-on-error (&body body)
  "Use this to wrap the scanning process and it will handle scan errors by inserting the special `@invalid' token and continue scanning at the next token"
  `(handler-bind ((invalid-token #'skip-to-next-token))
     ,@body))

(defun skip-to-next-token (c)
  "Invoke the restart `skip-to-next-token' to skip to the next token"
  (declare (ignore c))
  (let ((restart (find-restart 'skip-to-next-token)))
    (when restart
      (invoke-restart restart))))

(defun next-token (lexer)
  "Attempt to scan the next token and return it. This is the main interface to the lexer.
Example:
  
    (defvar lexer (make-lexer \"1 + 2\")
    (next-token lexer)
If an error is encountered it will signal an `invalid-token' condition.
You can use the `skip-on-error' macro to handle errors and continue scanning.

  (skip-on-error
    (next-token lexer)))

If you skip the error, the token stream will include a special `@ignored' token for the part of the input that has been skipped.
"
  (restart-case (scan-token lexer)
    (skip-to-next-token ()
      (incf (slot-value lexer 'error-count))
      (recover lexer)
      (accept lexer @ignored))))

(defun peek (lexer)
  "Peeks at the next character in the input string."
  (with-slots (look-ahead) lexer
    (source:cursor-value look-ahead)))

(defun advance-while (lexer predicate)
  "Scans the input string while the predicate is true."
  (loop
    :for c = (peek lexer)
    :while (and c (funcall predicate c))
    :do (advance lexer)))

(defun advance-until (lexer predicate)
  "Scans the input string until the predicate is true."
  (loop :for c = (peek lexer)
    :while (and c (not (funcall predicate c)))
    :do (advance lexer)))

(defun skip-until (lexer predicate)
  "Skips the input string until the predicate is true."
  (loop
    :for c = (peek lexer)
    :while (and c (not (funcall predicate c)))
    :do (advance lexer)
    :finally (skip lexer)))

(defun skip-while (lexer predicate)
  "Skips the input string while the predicate is true."
  (skip-until lexer (complement predicate)))

(defun eofp (lexer)
  "Returns true if the lexer has reached the end of the input."
  (with-slots (look-ahead) lexer
    (source:cursor-eofp look-ahead)))

(defun advance (lexer)
  "Advances the look-ahead cursor by one and returns the character at the position it pointed to"
  (with-slots (look-ahead) lexer
    (prog1 (source:cursor-value look-ahead)
      (source:cursor-advance look-ahead))))

(defun retreat (lexer)
  "Retreats the look-ahead cursor by one and returns the character at the position it pointed to"
  (with-slots (look-ahead) lexer
    (source:cursor-retreat look-ahead)))

(defun accept (lexer token-class &key transform-value include-lexeme)
  "Accepts the input between the `base' and `look-ahead' cursor as the next token with the provided token-class.
This will advance the `base' cursor to the `look-ahead' cursor and return the token.
"
  (with-slots (base look-ahead) lexer
    (let* ((lexeme (source:cursor-value base :end-cursor look-ahead))
            (token (token token-class (and include-lexeme lexeme) (and transform-value (funcall transform-value lexeme)) (source:cursor-position base))))
      (setf base (source:cursor-clone look-ahead))
      token)))

(defun current-lexeme (lexer)
  "Returns the current lexeme between the `base' and `look-ahead' cursor."
  (with-slots (base look-ahead) lexer
    (source:cursor-value base :end-cursor look-ahead)))

(defun backtrack (lexer)
  "Backtracks the look-ahead cursor to base."
  (with-slots (base look-ahead) lexer
    (setf look-ahead (source:cursor-clone base))))

(defun skip (lexer)
  "Skips the input between the `base' and `look-ahead' cursor."
  (with-slots (base look-ahead) lexer
    (setf base (source:cursor-clone look-ahead))))
