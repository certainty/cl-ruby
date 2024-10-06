(in-package :cl-ruby.parser)

;;; This parser is based on the following BNF grammar for Ruby:
;;; https://cse.buffalo.edu/~regan/cse305/RubyBNF.pdf
;;;
;;; However we do deviate in the way we encode the productions some times. 

(defun $expr (state)
  ($arg state))

(defun $arg (state)
  ($primary state))

(defun $primary (state)
  ($literal state))

;;; LITERAL : numeric | SYMBOL | STRING | STRING2 | HERE_DOC | REGEXP
(defun $literal (state)
  ($numeric state))

(defun $numeric (state)
  (let ((token (expect state @number)))
    (accept state (make-literal token))))



