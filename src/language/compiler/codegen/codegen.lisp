(in-package :cl-ruby.codegen)

;; the codegenerator builds the common lisp s-expressions from our ruby AST
;; (loop :for expr :in (codegen::emit-sexps (parse "1234")) :collect (eval expr))


(defclass state ()
  ((chunk
     :initarg :chunk
     :initform nil
     :type list)))

(defun emit-sexps (ast)
  (let ((state (make-instance 'state)))
    (with-slots (chunk) state
      (emit state ast)
      (reverse chunk))))

(defgeneric emit (state node)
  (:documentation "Emit a node into the state"))

(defmethod emit ((state state) (node parser::literal))
  ;; emit a literal
  (emit state (parser::ast-literal-token node)))

(defmethod emit ((state state) (node lexer:token))
  (cond
    ((lexer:token-class= node lexer::@number)
      (push (sym (lexer::token-value node)) (slot-value state 'chunk)))
    (t (error "no other token supported"))))

(defgeneric sym (value))

(defmethod sym ((value integer))
  `',value)
