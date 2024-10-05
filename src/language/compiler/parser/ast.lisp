(in-package :cl-ruby.parser)

(defclass <ast-node> () ()
  (:documentation "An AST node is a node in the abstract syntax tree that represents a part of the program."))

(defclass <expression> (ast-node) ()
  (:documentation "An expression is a node in the AST that represents a computation."))

(defclass <literal> (expression)
  ((token
     :reader literal-token
     :initform (a:required-argument "token")
     :initarg :token)
    (value
      :reader literal-value
      :initarg :value))
  (:documentation "A literal is a node in the AST that represents a constant value."))

(defgeneric convert-literal-value (token)
  (:documentation "Returns the value of a literal token."))

(defmethod initialize-instance :after ((literal <literal>) &key)
  (with-slots (token value) literal
    (setf value (literal-value token))))

