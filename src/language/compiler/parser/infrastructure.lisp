(in-package :cl-ruby.parser)

(defclass state ()
  ((scanner
     :initarg :scanner
     :initform (a:required-argument "lexer")
     :type lexer)
    (panic-mode
      :initform nil
      :type boolean)))


;; Parser Interface
(define-condition parser-error (error) ())

(define-condition parse-failure (parser-error)
  ((message
     :initarg :message
     :initform "parse error"
     :type string)
    (state
      :initarg :state
      :initform nil
      :type state)))

(defun parse (input &key (rule '<expression))
  (cl-ruby.source:with-source-code input #'(lambda (stream) (parse-source stream :rule rule))))

(defun parse-source (source-code &key (rule '<expression))
  (let ((state (make-instance 'state :scanner (make-lexer source-code))))
    (parse* state rule)))

(defun parse* (state rule)
  (restart-case (funcall rule)
    (synchronize ()
      :report "Attempt to continue parsing after the next statement boundary"
      (ignore-errors (synchronize state)))))

(defun synchronize (state)
  (declare (ignore state))
  nil)

(defmacro collecting-errors (&body body)
  "Enables error recovery in which the parser does not fail after the first error.
Instead it tries to recover and continue parsing.
It collects all errors and returns the following values
  - the result of the parser
  - a list of errors "
  (let ((errors (gensym))
         (result (gensym))
         (condition (gensym)))
    `(let* ((,errors nil)
             (,result (handler-bind ((parse-failure
                                       (lambda (,condition)
                                         (push ,condition ,errors)
                                         (when (find-restart 'synchronize)
                                           (invoke-restart 'synchronize)))))
                        ,@body)))
       (values (if ,errors nil ,result) ,errors))))

