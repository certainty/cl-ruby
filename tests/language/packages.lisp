(in-package :cl-user)

(defpackage :cl-ruby.tests.language.lexer
  (:use :cl :parachute :cl-ruby.lexer)
  (:local-nicknames
    (:source :cl-ruby.source)))

(defpackage :cl-ruby.tests.language.compiler
  (:use :cl :parachute :cl-ruby.compiler)
  (:local-nicknames
    (:source :cl-ruby.source)))

(defpackage :cl-ruby.tests.language
  (:use :cl :parachute)
  (:export :run-all-tests))

