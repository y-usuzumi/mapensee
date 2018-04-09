p (1..10).count { |number| number.even? }  # 5
p (1..10).select { |number| number.even? }  # [2, 4, 6, 8, 10]
p (1..10).any? { |number| number < 8 }  # true
p (1..10).all? { |number| number < 8 }  # false
(1..5).each do |number|
  if number.even?
    puts "#{number} is event"
  else
    puts "#{number} is odd"
  end
end
p (1..10).map { |number| number * 3 }
p (1..10).select(&:even?)
p ['one', 'two', 'three'].map(&:upcase)

p ['one', 'two', 'three'].map(&:chars)
# [["o", "n", "e"], ["t", "w", "o"], ["t", "h", "r", "e", "e"]]
p ['one', 'two', 'three'].flat_map(&:chars)
# ["o", "n", "e", "t", "w", "o", "t", "h", "r", "e", "e"]

# quite like foldl
p (1..10).inject(0) { |result, number| result + number }  # 55
p (1..10).inject(1) { |result, number| result * number }  # 3628800
p ['one', 'two', 'three'].inject('Words: ') { |result, word| "#{result} #{word}" }
# "Words: one two three"
