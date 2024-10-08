(in-package :cl-user)

(defpackage :cl-ruby.source
  (:use :cl)
  (:local-nicknames
    (:s :serapeum)
    (:a :alexandria))
  (:export
    :source-position
    :source-origin
    :format-position
    :source-code
    :source-code-stream
    :with-source-code
    :open-source-code
    :close-source-code
    :origin-designator))

(defpackage :cl-ruby.lexer
  (:use :cl)
  (:local-nicknames
    (:s :serapeum)
    (:a :alexandria)
    (:source :cl-ruby.source))
  (:shadow :class)
  (:export
    :make-lexer
    :scan-token
    :token-class
    :token-lexeme
    :token-position
    :token-bind
    :@eof
    :@illegal
    :@number))

(defpackage :cl-ruby.parser
  (:use :cl :cl-ruby.lexer)
  (:local-nicknames
    (:s :serapeum)
    (:a :alexandria)
    (:source :cl-ruby.source))
  (:export
    :parse-error
    :parse-failure
    :parse
    :collecting-errors
    :<ast-node>
    :<expr>
    :<literal>
    :literal-token
    :literal-value
    :make-literal))

(defpackage :cl-ruby.codegen
  (:use :cl :cl-ruby.parser)
  (:local-nicknames
    (:s :serapeum)
    (:a :alexandria)
    (:source :cl-ruby.source)
    (:parser :cl-ruby.parser))
  (:export
    :generate))

(defpackage :cl-ruby.compiler
  (:use :cl)
  (:local-nicknames
    (:s :serapeum)
    (:a :alexandria)
    (:source :cl-ruby.source)
    (:lexer :cl-ruby.lexer)
    (:parser :cl-ruby.parser)
    (:codegen :cl-ruby.codegen))
  (:export
    :transpile))
