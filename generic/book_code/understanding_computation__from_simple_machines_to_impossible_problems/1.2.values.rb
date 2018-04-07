## Basic values

# booleans, numbers and strings
p (true && false) || true
p (3+3) * (14/2)
p 'hello' + ' world'
p 'hello world'.slice(6)

# symbols
p :my_symbol
p :my_symbol == :my_symbol
p :my_symbol == :another_symbol
p 'hello world'.slice(11)


## Data structures

# arrays
numbers = ['zero', 'one', 'two']
p numbers[1]
numbers.push('three', 'four')
p numbers
numbers.drop(2)
p numbers

# ranges
ages = 18..30
p ages.entries
p ages.include?(25)
p ages.include?(33)

# hashes

fruit = {'a' => 'apple', 'b' => 'banana', 'c' => 'coconut'}
p fruit['b']
fruit['d'] = 'date'
p fruit

# (also often times symbols are used as keys of hashes)

dimensions = {width: 1000, height: 2250, depth: 250}  # same as {:width => 1000, :height => 2250, :depth => 250}
p dimensions[:depth]


## proc
# procs are a chunk of unevaluated ruby code
multiply = -> x, y { x * y }
p multiply.call(6, 9)
p multiply.call(2, 3)
p multiply[3, 4]  # same as `.call`
