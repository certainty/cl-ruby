(in-package :cl-ruby.parser)

(defclass state ()
  ((tokens
     :initarg :tokens
     :initform (error "required argument :tokens not provided")
     :type (vector lexer:token *))
    (save-points
      :initarg :save-points
      :initform nil
      :type list)
    ;; do I need this?
    (base
      :initarg :base
      :initform 0
      :type integer)
    (look-ahead
      :initarg :look-ahead
      :initform 0
      :type integer)
    (panic-mode
      :initform nil
      :type boolean)))


;; Parser Interface

(defun make-state (tokens)
  (make-instance 'state :tokens tokens))

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

(defparameter *state* nil "State is bound during parsing to the current state")

(defun parse (input &key (rule '<expression))
  (let* ((tokens (lexer:tokenize input))
          (state (make-state tokens)))
    (parse* state rule)))

(defun parse* (state rule)
  (let ((*state* state))
    (restart-case (funcall rule)
      (synchronize ()
        :report "Attempt to continue parsing after the next statement boundary"
        (ignore-errors (synchronize state))))))

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

;; primitive parse macros
(defun .satisfies (predicate &optional (state *state*))
  "Returns t or nil depending on whether the next token satisfies the predicate."
  (unless (eofp state)
    (funcall predicate (peek state))))

(defmacro .try (&body body)
  `(handler-case
       (progn
         (add-save-point *state*)
         ,@body)
       (parse-failure (e)
         (restore-save-point *state*)
         nil)))

(defmacro .or (parser &rest parsers)
  (if parsers
    `(or (.try (,parser))
       (.or ,@parsers))
    `(,parser)))

(defun synchronize (&optional (state *state*))
  "Try to synchronize the parser to the next statement boundary"
  (with-slots (panic-mode) state
    (setf panic-mode t)))

(defun add-save-point (&optional (state *state*))
  (with-slots (look-ahead save-points) state
    (push look-ahead save-points)))

(defun restore-save-point (&optional (state *state*))
  (with-slots (look-ahead save-points) state
    (setf look-ahead (pop save-points))))

(defun commit-save-point (&optional (state *state*))
  (with-slots (look-ahead save-points) state
    (pop save-points)))

(defun advance (&optional (state *state*))
  (with-slots (look-ahead tokens) state
    (prog1 (aref tokens look-ahead)
      (incf look-ahead))))

(defun accept (&optional (state *state*))
  (with-slots (tokens look-ahead base save-points) state
    ;; return the tokens from base to look-ahead as a view into the tokens array
    ;; we'll use a displaced array for that
    (prog1 (if (= look-ahead base)
             (list (peek))
             (coerce (subseq tokens base look-ahead) 'list))
      (setf base look-ahead)
      (commit-save-point))))

(defun fail (message &optional (state *state*))
  (error 'parse-failure :message message :state state))

(defun eofp (&optional (state *state*))
  (with-slots (look-ahead tokens) state
    (>= look-ahead (length tokens))))

(defun peek (&optional (state *state*))
  (with-slots (look-ahead tokens) state
    (unless (eofp state)
      (aref tokens look-ahead))))
