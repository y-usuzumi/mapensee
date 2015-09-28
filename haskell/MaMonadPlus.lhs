In our studies so far, we saw that the Maybe and list monads both represent the number of results a computation can have. That is, you use Maybe when you want to indicate that a computation can fail somehow (i.e. it can have 0 results or 1 result), and you use the list monad when you want to indicate a computation could have many valid answers ranging from 0 results to many results.

Given two computations in one of these monads, it might be interesting to amalgamate all valid solutions into a single result. For example, within the list Monad, we can concatenate two lists of valid solutions.

Definition
====
MonadPlus defines two methods. mzero is the monadic value standing for zero results; while mplus is a binary function which combines two computations.

> module Main where
>
> import Test.Hspec
> import Control.Monad
>
> main :: IO ()
> main = hspec $ do
>   describe "MonadPlus for Maybe monad" $ do
>     it "should be Nothing" $
>       (mzero :: Maybe Int) `shouldBe` Nothing
>     it "should be the first non-empty value" $ do
>       Nothing `mplus` Just 3 `shouldBe` Just 3
>       Just 3 `mplus` Nothing `shouldBe` Just 3

The Maybe monad cannot have more than one solution, thus discarding the second one.

>       Just 3 `mplus` Just 4 `shouldBe` Just 3
>
>   describe "MonadPlus for [] monad" $ do
>     it "should be an empty list" $
>       (mzero :: [Int]) `shouldBe` []
>     it "should be the first non-empty value" $ do
>       [] `mplus` [1, 2, 3] `shouldBe` [1, 2, 3]
>       [1, 2, 3] `mplus` [] `shouldBe` [1, 2, 3]
>     it "should concat the two lists if both not empty" $
>       [1, 2, 3] `mplus` [6, 5, 4] `shouldBe` [1, 2, 3, 6, 5, 4]
