(in-package :cl-ruby.parser)

(defclass ast-node () ())

(defclass literal (ast-node)
  ((token
     :reader ast-literal-token
     :initarg :token)))

(defun .is (token-class)
  #'(lambda (token)
      (lexer:token-class= token token-class)))

(defun <expression ()
  (<literal))

(defun <literal ()
  (cond
    ((.satisfies (.is lexer::@number))
      (destructuring-bind (num) (accept)
        (make-instance 'literal :token num)))
    (t (fail "Expected a literal"))))



