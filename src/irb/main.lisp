(in-package :cl-ruby.irb.main)

(defun irb/handler (cmd)
  (declare (ignorable cmd))
  (format t "Welcome to the CL-Ruby IRB shell.~%"))

(defun irb/command ()
  "Start an interactive Ruby shell."
  (clingon:make-command
    :name "irb"
    :description "Interactive Ruby shell for CL-Ruby"
    :version "0.1.0" 
    :authors '("David Krentzlin")
    :options nil
    :handler #'irb/handler))

(defun main ()
  (let ((app (irb/command)))
    (clingon:run app)))
