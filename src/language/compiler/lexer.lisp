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
(deftoken-class* @op_scope @op_&& @op_|| @op_not @op_? @op_= @op_== @op_=== @op_<=> @op_+ @op_+= @op_- @op_-= @op_/ @op_* @op_** @op_*= @op_%)

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
     :documentation "The input that we are scanning."))
  (:documentation "The lexer is responsible for scanning the input string and returning tokens.
   It keeps track of the current position in the input string, as well as the current line and column number.
   The lexer is also responsible for handling errors, such as illegal tokens."))

(defun make-lexer (source-code)
  "Creates a new lexer for the given input string."
  (make-instance 'lexer :input source-code))

(defun scan-token (lexer &key (mode :ruby))
  (declare (ignore lexer mode))
  nil)

