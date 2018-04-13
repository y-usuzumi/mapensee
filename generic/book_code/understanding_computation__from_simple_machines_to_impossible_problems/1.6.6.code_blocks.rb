# yield calls the code block
def do_three_times
  yield
  yield
  yield
end

p do_three_times { puts 'hello' }

# give arguments
def do_three_times
  yield 'first'
  yield 'second'
  yield 'third'
end

p do_three_times { |n| puts "#{n}: hello" }

# `yield` returns the result of the code block
def number_names
  [yield('one'), yield('two'), yield('three')].join(',')
end

p number_names { |name| name.upcase.reverse }  # "ENO, OWT, EERHT"


# a function can receive optional code block.
# use `.block_given?` to test.

# also, add an `&block` parameter if you want a reference to the block
# i.e. def my_method(&block)

# `.map(&:upcase)` is the same as `.map { |char| char.upcase }`
