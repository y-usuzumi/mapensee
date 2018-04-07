## Classes

class Calculator
  def divide(x, y)
    x / y
  end
end

c = Calculator.new
p c.class  # Calculator
p c.divide(10, 2)  # 5


# inheritance
# The parent goes to the opening side of <
class MultiplyingCalculator < Calculator
  def multiply(x, y)
    x * y
  end
end

mc = MultiplyingCalculator.new
p mc.class  # MultiplyingCalculator
p mc.class.superclass  # Calculator

p mc.multiply(10, 2)  # 20
p mc.divide(10, 2)


class BinaryMultiplyingCalculator < MultiplyingCalculator
  def multiply(x, y)
    # super to call the method of the same name in its parent
    result = super(x, y)
    result.to_s(2)
  end
end

bmc = BinaryMultiplyingCalculator.new
p bmc.multiply(10, 2)  # "10100"


## Modules

module Addition
  def add(x, y)
    x + y
  end
end


class AddingCalculator
  include Addition
end


ac = AddingCalculator.new
p ac.add(10, 2)  # 12
