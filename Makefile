.PHONY: all cl-ruby cl-irb language

LISP ?= sbcl --noinform

all: bin/cl-ruby bin/cl-irb 

bin/cl-ruby:
	$(LISP) --eval '(ql:quickload :cl-ruby)' --eval '(asdf:make :cl-ruby/rubyc)' --eval '(quit)' && mv src/rubyc/cl-ruby bin/cl-ruby

bin/cl-irb:
	$(LISP) --eval '(ql:quickload :cl-ruby)' --eval '(asdf:make :cl-ruby/irb)' --eval '(quit)' && mv src/irb/cl-irb bin/cl-irb

language:
	$(LISP) --eval '(ql:quickload :language)'

clean:
	rm bin/cl-ruby bin/cl-irb
