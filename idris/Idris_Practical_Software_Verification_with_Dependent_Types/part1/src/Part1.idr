-- Part 1: Basic Idris

module Main

import Data.Vect

{- 1. Write a function `repeat : (n : Nat) -> a -> Vect n a` which constructs a vector of `n` copies of an item.
 -}
repeat : (n : Nat) -> a -> Vect n a
repeat Z _ = Nil
repeat (S n) a = a :: repeat n a

{- 2. Consider the following functions over `List`s:

      ```idris
      take : Nat -> List a -> List a
      drop : Nat -> List a -> List a
      ```

      (a) What are the types of the corresponding functions for `Vect`, `vtake` and `vdrop`?
          Hint: What are the invariants? i.e. how many items need to be in the vector in each case?

      (b) Implement `vtake` and `vdrop`.
 -}

vtake : (n : Nat) -> Vect (n + m) a -> Vect n a
vtake Z v = Nil
vtake (S n) (h :: v) = h :: vtake n v

vdrop : (n : Nat) -> Vect (n + m) a -> Vect m a
vdrop Z v = v
vdrop (S n) (_ :: v) = vdrop n v

{- 3. A matrix is a 2-dimensional vector, and could be defined as follows:

      ```idris
      Matrix : Type -> Nat -> Nat -> Type
      Matrix a n m = Vect n (Vect m a)
      ```

      (a) Using `repeat`, above, and `Vect.zipWith`, write a function which transposes a matrix.
          Hints: Remember to think carefully about its type first! `zipWith` for vectors is defined as follows:

          ```idris
          zipWith : (a -> b -> c) -> Vect n a -> Vect n b -> Vect n c
          zipWith f [] [] = []
          zipWith f (x :: xs) (y :: ys) = f x y :: zipWith f xs ys
          ```

      (b) Write a function to multiply two matrices.
 -}

Matrix : Type -> Nat -> Nat -> Type
Matrix a n m = Vect n (Vect m a)

transpose : Matrix a m n -> Matrix a n m
transpose Nil = repeat n Nil
transpose (r::rs) = zipWith (::) r (transpose rs)

main : IO ()
main = do
  let v : Vect (fromInteger 5) Int = 1 :: 2 :: 3 :: 4 :: 5 :: Nil
  putStrLn $ show $ repeat 3 "OK"
  putStrLn $ show $ vdrop 3 v
  putStrLn $ show $ vtake 3 v
  putStrLn $ transpose $ repeat 2 v

