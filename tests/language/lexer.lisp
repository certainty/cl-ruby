(in-package :cl-ruby.tests.language.lexer)

(defun scan (input)
  (cl-ruby.source:with-source-code (s (cl-ruby.source:from-string input))
    (let ((scanner (make-lexer s)))
      (scan-token scanner))))

(defmacro scans-as (input cls &optional lexeme)
  (let ((tok (gensym)))
    `(let ((,tok (scan ,input)))
       (false (null ,tok))
       (is eq (token-class ,tok) ,cls)
       ,(when lexeme
         `(is equal (token-lexeme ,tok) ,lexeme)))))

(define-test scan-integer
  "Scan integer literal"
  (scans-as "123" @number "123"))

(define-test scan-float
  "Scan float literal"
  (scans-as "123.99" @number "123.99"))

