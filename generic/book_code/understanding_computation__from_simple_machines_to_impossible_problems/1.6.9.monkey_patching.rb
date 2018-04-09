class Point < Struct.new(:x, :y)
  def +(other_point)
    Point.new(x + other_point.x, y + other_point.y)
  end

  def inspect
    "#<Point (#{x}, #{y})>"
  end
end

# extend the defined class
class Point
  def -(other_point)
    Point.new(x - other_point.x, y - other_point.y)
  end
end

p Point.new(10, 15) - Point.new(1, 1)  # #<Point (9, 14)>


# we can also extend builtin classes:
class String
  def shout
    upcase + '!!!'
  end
end

p 'hello world'.shout
