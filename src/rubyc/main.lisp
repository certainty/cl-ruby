(in-package :cl-ruby.rubyc.main)


(defun rubyc/handler (cmd)
  (declare (ignorable cmd))
  (format t "Welcome to cl-ruby"))

(defun rubyc/command ()
  "The main rubyc command"
  (clingon:make-command
    :name "cl-ruby"
    :description "Interactive Ruby shell for CL-Ruby"
    :version "0.1.0" 
    :authors '("David Krentzlin")
    :options nil
    :handler #'rubyc/handler))

(defun main ()
  (let ((app (rubyc/command)))
    (clingon:run app)))

