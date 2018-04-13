# if-else
p (
    if 2 < 3
      'less'
    else
      'more'
    end
  )  # 'less'


# case-when
quantify =
  -> number {
  case number
  when 1
    'one'
  when 2
    'a couple'
  else
    'many'
  end
  }
p quantify.call(2)  # a couple
p quantify.call(10)  # many

# while
x = 1
while x < 1000
  x = x * 2
end
p x  # 1024
