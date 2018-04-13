def join_with_commas(*words)
  words.join(', ')
end

p join_with_commas('one', 'two', 'three')  # "one, two, three"


# A method def can only have one variadic parameter.
# It can come before or after regular parameters.
def join_with_commas(before, *words, after)
  before + words.join(', ') + after
end

p join_with_commas('Testing: ', 'one', 'two', 'three', '.')

# Almost the same with Python
arguments = ['Testing: ', 'one' ,'two', 'three', '.']
p join_with_commas(*arguments)

# multiple assignment
before, *words, after = ['Testing: ', 'one', 'two', 'three', '.']
p before  # "Testing: "
p words  # ["one", "two", "three"]
p after  # "."
