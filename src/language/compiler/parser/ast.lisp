(in-package :cl-ruby.parser)

(defclass <ast-node> () ()
  (:documentation "An AST node is a node in the abstract syntax tree that represents a part of the program."))

(defclass <expr> (<ast-node>) ()
  (:documentation "An expression is a node in the AST that represents a computation."))

(defclass <literal> (<expr>)
  ((token
     :reader literal-token
     :initform (a:required-argument "token")
     :initarg :token)
    (value
      :reader literal-value
      :initarg :value))
  (:documentation "A literal is a node in the AST that represents a constant value."))

(defun make-literal (token)
  (make-instance '<literal> :token token))

(defmethod initialize-instance :after ((literal <literal>) &key)
  (with-slots (token value) literal
    (setf value (convert-literal-value token))))

(defun convert-literal-value (token)
  (token-bind (cls lexeme) token
    (s:select cls
      (@number (parse-integer lexeme)))))
