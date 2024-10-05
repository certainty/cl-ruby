(in-package :cl-ruby.parser)

(defclass state ()
  ((scanner
     :initarg :scanner
     :initform (a:required-argument "lexer")
     :type lexer)
    (previous
      :initform nil
      :type (or null token))
    (current
      :initform nil
      :type (or null token))
    (had-errors
      :initform nil
      :type boolean)
    (panic-mode
      :initform nil
      :type boolean)))

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

(defun parse (stream &key (rule '<expression))
  (let ((state (make-instance 'state :scanner (make-lexer stream))))
    (with-slots (had-errors panic-mode) state
      (setf had-errors nil panic-mode nil)
      (advance state)
      (let ((ast (funcall rule state)))
        (expect state @eof)
        (values ast had-errors)))))

(defun synchronize (state)
  (declare (ignore state))
  nil)

(defun advance (state)
  "Reads the next token from the input stream and updates `previous` and `current` accordingly.
If the next token is an illegal token, a parser error is raised. "
  (with-slots (scanner previous current) state
    (setf previous current current (scan-token scanner))
    (when (eq (token-class current) @illegal)
      (raise-parser-error state "Illegal token"))))

(defun expect (state cls)
  (with-slots (current) state
    (unless (and current (eq (token-class current) cls))
      (raise-parser-error state "Expected ~A but found ~A" cls current))
    (advance state)
    t))

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

(defun parse-failure (state message)
  (restart-case (error 'parse-failure :message message :state state)
    (synchronize ()
      :report "Attempt to continue parsing after the next statement boundary"
      (ignore-errors (synchronize state)))))

(defun raise-parser-error (state fmt &rest fmt-args)
  (with-slots (current panic-mode had-errors) state
    (unless panic-mode ; in panic mode we ignore all errors
      (setf panic-mode t had-errors t)
      (restart-case (error 'parse-failure :message (apply #'format nil fmt fmt-args) :state state)
        (synchronize ()
          :report "Attempt to continue parsing after the next statement boundary"
          (ignore-errors (synchronize state)))))))
