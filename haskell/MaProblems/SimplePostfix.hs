module Postfix where

begin f = f []

push vm x f = f (x:vm)

add (x:y:vm) f = f (x+y:vm)

end = head
