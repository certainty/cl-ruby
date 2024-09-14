(in-package :asdf-user)

(defsystem :cl-ruby
  :description "A Ruby implementation that targets the Common Lisp platform"
  :author "David Krentzlin"
  :maintainer "David Krentzlin"
  :source-control (:git "https://github.com/certainty/cl-ruby")
  :version "0.1"
  :depends-on (:serapeum

                :str
                :cl-ppcre

                :maxpc)
  :serial t
  :pathname "src/language"
  :components
  ((:file "packages")
    (:module "compiler"
      :components
      ((:file "parser")))))

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
