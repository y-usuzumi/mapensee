## Expressions

class Number < Struct.new(:value)
end


class Add < Struct.new(:left, :right)
end


class Multiply < Struct.new(:left, :right)
end


# Test
p Add.new(
    Multiply.new(Number.new(1), Number.new(2)),
    Multiply.new(Number.new(3), Number.new(4))
  )


# For readability in IRB:
class Number
  def to_s
    value.to_s
  end

  def inspect
    "<<#{self}>>"
  end
end


class Add
  def to_s
    "#{left} + #{right}"
  end

  def inspect
    "#{self}"
  end
end


class Multiply
  def to_s
    "#{left} * #{right}"
  end

  def inspect
    "<<#{self}>>"
  end
end


# Test
p Add.new(
    Multiply.new(Number.new(1), Number.new(2)),
    Multiply.new(Number.new(3), Number.new(4))
  )

p Number.new(5)

p Multiply.new(
    Number.new(1),
    Multiply.new(
      Add.new(Number.new(2), Number.new(3)),
      Number.new(4)
    )
)


# Check reducible

class Number
  def reducible?
    false
  end
end


class Add
  def reducible?
    true
  end
end


class Multiply
  def reducible?
    true
  end
end


# Reduce logic

class Add
  def reduce
    if left.reducible?
      Add.new(left.reduce, right)
    elsif right.reducible?
      Add.new(left, right.reduce)
    else
      Number.new(left.value + right.value)
    end
  end
end


class Multiply
  def reduce
    if left.reducible?
      Multiply.new(left.reduce, right)
    elsif right.reducible?
      Multiply.new(left, right.reduce)
    else
      Number.new(left.value * right.value)
    end
  end
end


# Test
expression =
  Add.new(
    Multiply.new(Number.new(1), Number.new(2)),
    Multiply.new(Number.new(3), Number.new(4))
  )
p expression
p expression.reducible?
p expression = expression.reduce
p expression.reducible?
p expression = expression.reduce
p expression.reducible?
p expression = expression.reduce
p expression.reducible?


# MACHINE!
class Machine < Struct.new(:expression)
  def step
    self.expression = expression.reduce
  end

  def run
    puts expression
    begin
      step
      puts expression
    end while expression.reducible?
  end
end


# Test

expression =
  Add.new(
    Multiply.new(Number.new(1), Number.new(2)),
    Multiply.new(Number.new(3), Number.new(4))
  )

machine = Machine.new expression
machine.run


# Logical operations

class Boolean < Struct.new(:value)
  def to_s
    value.to_s
  end

  def inspect
    "<<#{self}>>"
  end

  def reducible?
    false
  end
end


class LessThan < Struct.new(:left, :right)
  def to_s
    "#{left} < #{right}"
  end

  def inspect
    "<<#{self}>>"
  end

  def reducible?
    true
  end

  def reduce
    if left.reducible?
      LessThan.new(left.reduce, right)
    elsif right.reducible?
      LessThan.new(left, right.reduce)
    else
      Boolean.new(left.value < right.value)
    end
  end
end


# Test
machine =
  Machine.new(
    LessThan.new(
      Number.new(5),
      Add.new(Number.new(2), Number.new(2))
    )
  )
machine.run


# VARIABLES!

class Variable < Struct.new(:name)
  def to_s
    name.to_s
  end

  def inspect
    "<<#{self}>>"
  end

  def reducible?
    true
  end
end

# Now that variables require an 'environment',
# We gotta redefine all previously defined `reduce` methods:

class Add
  remove_method :reduce

  def reduce(environment)
    if left.reducible?
      Add.new(left.reduce(environment), right)
    elsif right.reducible?
      Add.new(left, right.reduce(environment))
    else
      Number.new(left.value + right.value)
    end
  end
end


class Multiply
  remove_method :reduce

  def reduce(environment)
    if left.reducible?
      Multiply.new(left.reduce(environment), right)
    elsif right.reducible?
      Multiply.new(left, right.reduce(environment))
    else
      Number.new(left.value * right.value)
    end
  end
end


class LessThan
  remove_method :reduce

  def reduce(environment)
    if left.reducible?
      LessThan.new(left.reduce(environment), right)
    elsif right.reducible?
      LessThan.new(left, right.reduce(environment))
    else
      Boolean.new(left.value < right.value)
    end
  end
end


class Variable
  def reduce(environment)
    environment[name]
  end
end


# Redefine Machine
Object.send(:remove_const, :Machine)

class Machine < Struct.new(:expression, :environment)
  def step
    self.expression = expression.reduce(environment)
  end

  def run
    puts expression
    begin
      step
      puts expression
    end while expression.reducible?
  end
end


# Test

machine =
  Machine.new(
    Add.new(Variable.new(:x), Variable.new(:y)),
    { x: Number.new(3), y: Number.new(4) }
  )
machine.run


## Statements

class DoNothing
  def to_s
    'do-nothing'
  end

  def inspect
    "<<#{self}>>"
  end

  def ==(other_statement)
    other_statement.instance_of?(DoNothing)
  end

  def reducible?
    false
  end
end


class Assign < Struct.new(:name, :expression)
  def to_s
    "#{name} = #{expression}"
  end

  def inspect
    "<<#{self}>>"
  end

  def reducible?
    true
  end

  def reduce(environment)
    if expression.reducible?
      # Reducing the rhs does not change the environment
      [Assign.new(name, expression.reduce(environment)), environment]
    else
      # Assignment changes the environment
      [DoNothing.new, environment.merge({name => expression})]
    end
  end
end


# Test
statement = Assign.new(:x, Add.new(Variable.new(:x), Number.new(1)))
environment = { x: Number.new(2) }
p statement.reducible?
statement, environment = statement.reduce(environment)
p [statement, environment]
statement, environment = statement.reduce(environment)
p [statement, environment]
statement, environment = statement.reduce(environment)
p [statement, environment]
p statement.reducible?


# Redefine Machine to display the current statement and environment

Object.send(:remove_const, :Machine)

class Machine < Struct.new(:statement, :environment)
  def step
    self.statement, self.environment = statement.reduce(environment)
  end

  def run
    puts "#{statement}, #{environment}"
    begin
      step
      puts "#{statement}, #{environment}"
    end while statement.reducible?
  end
end


# Test

machine =
  Machine.new(
    Assign.new(:x, Add.new(Variable.new(:x), Number.new(1))),
    { x: Number.new(2) }
  )
machine.run


# TO-BE-CONTINUED
