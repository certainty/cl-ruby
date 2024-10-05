(in-package :cl-ruby.codegen)

;; the codegenerator builds the common lisp s-expressions from our ruby AST
;; (loop :for expr :in (codegen::emit-sexps (parse "1234")) :collect (eval expr))

(defclass state ()
  ((sexp
     :initarg :sexp
     :initform '(progn)
     :type list)
    (symbol-table
      :initarg :symbol-table
      :initform (a:required-argument "symbol-table"))))

(defun generate (ast symbol-table)
  (let ((state (make-instance 'state :symbol-table symbol-table)))
    (with-slots (sexp) state
      (emit-sexp state ast)
      (reverse sexp))))

(defgeneric emit-sexp (state ast)
  (:documentation "Emit a s-expression for the given AST node"))

(defmethod emit-sexp ((state state) (lit <literal>))
  (with-slots (sexp) state
    (push (literal-value lit) sexp)))
