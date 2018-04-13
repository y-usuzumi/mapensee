# Everything that starts with a capital letter is a constant!

NUMBERS = [4, 8, 15, 16, 23, 42]

# Can't do this!!!:
# NUMBERS = 123

class Greetings
  ENGLISH = 'hello'
  FRENCH = 'bonjour'
  GERMAN = 'guten Tag'
end

p NUMBERS.last  # 42
p Greetings::FRENCH  # "bonjour"
