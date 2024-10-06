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

(defmethod print-object ((state state) stream)
  (print-unreadable-object (state stream :type t)
    (with-slots (previous current had-errors panic-mode) state
      (format stream "previous: ~A~%" previous)
      (format stream "current: ~A~%" current)
      (format stream "had-errors: ~A~%" had-errors)
      (format stream "panic-mode: ~A~%" panic-mode))))

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

(defgeneric parse (input &key rule))

(defmethod parse ((input stream) &key (rule '$expr))
  (let ((state (make-instance 'state :scanner (make-lexer input))))
    (with-slots (had-errors panic-mode) state
      (setf had-errors nil panic-mode nil)
      (advance state)
      (let ((ast (funcall rule state)))
        (expect state @eof)
        (values ast had-errors)))))

(defmethod parse ((input source:source-code) &key (rule '$expr))
  (parse (source:source-code-stream input) :rule rule))

(defmethod parse ((origin source:source-origin) &key (rule '$expr))
  (source:with-source-code (s origin)
    (parse s :rule rule)))

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
    (prog1 current
      (advance state))))

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

(defun accept (state node)
  (declare (ignore state))
  (prog1 node
    ;; maybe do something later
    ))
