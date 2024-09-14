(in-package :cl-user)

(defpackage :cl-ruby.compiler
  (:use :cl)
  (:local-nicknames (:a :alexandria) (:s :serapeum))
  (:import-from :serapeum :->))
