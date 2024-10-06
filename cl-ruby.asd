(in-package :asdf-user)

(defsystem :cl-ruby
  :description "A Ruby implementation that targets the Common Lisp platform"
  :author "David Krentzlin"
  :maintainer "David Krentzlin"
  :source-control (:git "https://github.com/certainty/cl-ruby")
  :version "0.1"
  :in-order-to ((test-op (test-op "cl-ruby/tests")))
  :depends-on (:serapeum
                :alexandria
                :str
                :cl-ppcre
                )
  :serial t
  :pathname "src/language"
  :components
  ((:file "packages")
    (:module "compiler"
      :components
      ((:file "source")
        (:module "lexer"
          :components
         ((:file "token")
           (:file "scanner"))) 
       (:module "parser"
          :components
         ((:file "infrastructure")
           (:file "ast")
           (:file "language")))
       (:module "codegen"
          :components
         ((:file "codegen")))
        (:file "compiler")))))

(defsystem :cl-ruby/rubyc
  :description "The ruby compiler and interpreter"
  :depends-on (:cl-ruby :clingon)

  :build-operation "program-op"
  :build-pathname "cl-ruby"
  :entry-point "cl-ruby.rubyc.main:main"

  :serial t
  :pathname "src/rubyc"
  :components
  ((:file "packages")
    (:file "main")))

(defsystem :cl-ruby/irb
  :description "The interactive ruby shell"
  :depends-on (:cl-ruby :clingon)

  :build-operation "program-op"
  :build-pathname "cl-irb"
  :entry-point "cl-ruby.irb.main:main"

  :serial t
  :pathname "src/irb"
  :components
  ((:file "packages")
    (:file "main")))

(defsystem :cl-ruby/tests
  :description "The test suite for cl-ruby"
  :depends-on (:cl-ruby :parachute)
  :perform (test-op (op c)
             (declare (ignore op c))
                    (uiop:symbol-call :cl-ruby.tests.language :run-all-tests))
  :serial t
  :pathname "tests"
  :components
  ((:module "language"
     :components
     ((:file "packages")
       (:file "lexer")
       (:file "compiler")
       (:file "runner")))))
