(in-package :cl-user)

(defpackage :cl-ruby.source
  (:use :cl)
  (:nicknames :source)
  (:local-nicknames (:s :serapeum))
  (:export
    :source-position
    :source-origin
    :from-file
    :from-string
    :source-code
    :make-source-code
    :cursor
    :cursor-from-beginning
    :cursor-eofp
    :cursor-advance
    :cursor-retreat
    :cursor-value
    :cursor-clone
    :cursor-rewind
    :cursor-position))

(defpackage :cl-ruby.lexer
  (:use :cl)
  (:local-nicknames (:s :serapeum))
  (:shadow :class)
  (:export :lex))

(defpackage :cl-ruby.parser
  (:use :cl)
  (:export :parse))

