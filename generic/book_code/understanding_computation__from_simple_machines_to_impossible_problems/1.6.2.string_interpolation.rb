p "hello #{'dlrow'.reverse}"

o = Object.new
def o.to_s
  'a new object'
end
p "here is #{o}"  # here is a new object
