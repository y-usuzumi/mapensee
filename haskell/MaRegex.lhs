Regular Expressions:

> module Main where
>
> import Text.Regex.Posix
> import Test.Hspec
>
> main :: IO ()
> main = hspec $ do
>   describe "Regex" $ do
>     let s = "my left foot"
>     let rgx = "(foo|lef)"
>     it "matches with polymorphic return type" $ do

The =~ operator is polymorphic in its return type

>       s =~ rgx `shouldBe` True
>       s =~ rgx `shouldBe` "lef"
>       s =~ rgx `shouldBe` [["lef", "lef"], ["foo", "foo"]]
>       s =~ rgx `shouldBe` (2 :: Int)

!!! Watch out for String results
If you want a result that's a plain String, beware.
Since (=~) returns an empty string to signify “no match”, this poses an obvious difficulty
if the empty string could also be a valid match for the regexp. If such a case arises,
you should use a different return type instead, such as [[String]]
