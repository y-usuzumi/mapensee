o = Object.new
def o.add(x, y)
  x + y
end
p o.add(2, 3)

def o.add_twice(x, y)
  # Ruby tracks the current object (named `self`).
  # When sending messages to the current object,
  # you don't need to explicitly write the receiving object
  # i.e. o.add(x, y) + o.add(x, y)
  add(x, y) + add(x, y)
end

p o.add_twice(2, 3)

# Besides all method definitions, the current object is a special
# top-level object (namely `main`). All messages without specified receivers
# will be sent to main

def multiply(a, b)
  a * b
end

p multiply(2, 3)
