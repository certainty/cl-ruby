(in-package :cl-user)

(defpackage :cl-ruby.source
  (:use :cl)
  (:nicknames :source)
  (:local-nicknames (:s :serapeum) (:a :alexandria))
  (:export
    :source-position
    :source-origin
    :format-position
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
  (:nicknames :lexer)
  (:local-nicknames (:s :serapeum) (:a :alexandria))
  (:shadow :class)
  (:export
    :token
    :tokenize
    :token-class
    :token-class=
    :@lparen
    :@rparen
    ))

(defpackage :cl-ruby.parser
  (:use :cl :lexer)
  (:nicknames :parser)
  (:local-nicknames (:s :serapeum) (:a :alexandria)))

(defpackage :cl-ruby.codegen
  (:use :cl :parser)
  (:nicknames :codegen)
  (:local-nicknames (:s :serapeum) (:a :alexandria)))

