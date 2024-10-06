(in-package :cl-ruby.tests.language)

(defun run-all-tests ()
  (if (uiop:getenvp "CI_ENV")
      (progn
        (defvar cl-user::*exit-on-test-failures* t)
        (parachute:test :cl-ruby.tests.language.lexer :report 'plain)
        (parachute:test :cl-ruby.tests.language.compiler :report 'plain))
    (progn 
      (parachute:test :cl-ruby.tests.language.lexer :report 'interactive)
      (parachute:test :cl-ruby.tests.language.compiler :report 'interactive))))
