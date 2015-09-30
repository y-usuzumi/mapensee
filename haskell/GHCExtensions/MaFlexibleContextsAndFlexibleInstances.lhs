FlexibleContexts

In Haskell 98,

1. Contexts of type signatures, newtype and data declarations consist of assertions of the form
   C v or C (v t1 ... tn), where v is a type variable.
2. Contexts of instance and class declarations consist of assertions of the form C v,
   where v is a type variable. !! FIXME: This might be incorrect. Check FlexibleInstances below.

FlexibleContexts makes it possible to fill in arbitrary types as class arguments (in the contexts of
type signatures and class declarations).

E.g.
< g :: (C [a], D (a -> b)) => [a] -> b


FlexibleInstances

Same concept as the above, except that it applies to instance declarations.

E.g.
< class Something
<
< instance Something Int  -- Always works
< instance Something [Char]  -- Works only when FlexibleInstances is enabled
