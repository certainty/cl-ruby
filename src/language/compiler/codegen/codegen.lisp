(in-package :cl-ruby.codegen)

;; the codegenerator builds the common lisp s-expressions from our ruby AST
;; (loop :for expr :in (codegen::emit-sexps (parse "1234")) :collect (eval expr))

(defclass state ()
  ((sexp
     :initarg :sexp
     :initform nil
     :type list)))


(defun emit-sexp (ast symbol-table)
  (declare (ignore symbol-table ast))
  nil)
