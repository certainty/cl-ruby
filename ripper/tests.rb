require 'ripper'

source_code=<<-CODE

x = true
y = nil
x = 3
y = 4
x * y + 3

{ a: "foo", :b => 3}

module ExampleModule
  class ExampleClass
    attr_reader :foo

    class << self
      def lets_go(a, b, *c, **kw, &block)
        puts "Let's go"
        @a = a
        yield if block_given?
      end
    end
  end
end
CODE
pp Ripper.lex(source_code)
