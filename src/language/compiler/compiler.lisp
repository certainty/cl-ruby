(in-package :cl-ruby.compiler)

(defun transpile (origin)
 "Translate the Ruby source code which is read from `input' into Common Lisp source code.
  See `cl-ruby.source' which kinds of inputs are supported.
  In case of error this function signals a `compile-error'.
 "
  (source:with-source-code (s origin)
    (transpile-source s)))

(defun transpile-source (source-code)
  "Transpile the input which is represented by `source-code' into Common Lisp source code.
   See also `cl-ruby.source:source-code`"
   (multiple-value-bind (ast symbol-table) (parser:parse (source:source-code-stream source-code))
     (codegen:generate ast symbol-table)))
