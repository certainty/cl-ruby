(in-package :cl-ruby.tests.language.compiler)

(define-test transpile-literal-number
  "Transpile a single literal number"
  (is equal (transpile (source:from-string "123")) '(progn 123)))
