# Struct is a special class that can generate other classes

class Point < Struct.new(:x, :y)
  def +(other_point)
    Point.new(x + other_point.x, y + other_point.y)
  end

  def inspect
    "#<Point (#{x}, #{y})>"
  end
end

a = Point.new(2, 3)
b = Point.new(10, 20)
p a + b

# As with other methods we have defined, Point instances will respond to
# message '#x' and message '#x='
p a.x  # 2
p a.x = 35  # 35
p a + b  # #<Point (45, 24)>

# Also implemented is `#==`
p Point.new(4, 5) == Point.new(4, 5)  # true
p Point.new(3, 4) == Point.new(6, 7)  # false
