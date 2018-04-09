NUMBERS = [4, 8, 15, 16, 23, 42]

class Greetings
  ENGLISH = 'hello'
  FRENCH = 'bonjour'
  GERMAN = 'guten Tag'
end

p Object.send(:remove_const, :NUMBERS)  # [4, 8, 15, 16, 23, 42]
# Can't do `Object.remove_const(:NUMBERS)` because `remove_const`
# is a private method. Object.send workarounds this limitation

# Now the following line fails!
# p NUMBERS
