Type families permit a program to compute what data constructors it will operate on,
rather than having them fixed statically (as with simple type systems)
or treated as opaque unknowns (as with parametrically polymorphic types).

(1) Top level:
< -- Declare a list-like data family
< data family XList a

Notice that in the above example, XList has a default kind annotation: *
We can either add explicit kind annotation or list all parameters. E.g.
< data family XList :: * -> *
is a alternative to the above example.

< -- Declare a list-like instance for Char
< data instance XList Char = XCons !Char !(XList Char) | XNil
<
< -- Declare a number-like instance for ()
< data instance XList () = XListUnit !Int

(2) Inside type classes
< class GMapKey k where
<   data GMap k :: * -> *

In contrast to toplevel declarations, named arguments must be used for all type
parameters that are to be used as type-indices. Moreover, the argument names must be
class parameters. Each class parameter may only be used at most once per associated type, but some may be omitted and they may be in an order other than in the class head.
In other words: the named type parameters of the data declaration must be a permutation
of a subset of the class variables.

All data families must declared with the `data` keyword, while data instances can be
declared with either `data` or `newtype`, or a mix of both.

> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE ScopedTypeVariables #-}
>
> module Main where
>
> class (Show pokemon, Show (Move pokemon)) => Pokemon pokemon where
>   data Move pokemon :: *
>   pickMove :: pokemon -> Move pokemon
>
> data Fire = Charmander | Charmeleon | Charizard deriving Show
> instance Pokemon Fire where
>   data Move Fire = Ember | FlameThrower | FireBlast deriving Show
>   pickMove Charmander = Ember
>   pickMove Charmeleon = FlameThrower
>   pickMove Charizard = FireBlast
>
> data Water = Squirtle | Wartortle | Blastoise deriving Show
> instance Pokemon Water where
>   data Move Water = Bubble | WaterGun deriving Show
>   pickMove Squirtle = Bubble
>   pickMove _ = WaterGun
>
> data Grass = Bulbasaur | Ivysaur | Venusaur deriving Show
> instance Pokemon Grass where
>   data Move Grass = VineWhip deriving Show
>   pickMove _ = VineWhip
>
> printBattle :: String -> String -> String -> String -> String -> IO ()
> printBattle pokemonOne moveOne pokemonTwo moveTwo winner = do
>   putStrLn $ pokemonOne ++ " used " ++ moveOne
>   putStrLn $ pokemonTwo ++ " used " ++ moveTwo
>   putStrLn $ "Winner is: " ++ winner ++ "\n"
>
> class (
>   Show (Winner pokemon foe),
>   Pokemon pokemon,
>   Pokemon foe) => Battle pokemon foe where
>   type Winner pokemon foe :: *
>   type Winner pokemon foe = pokemon
>
>   pickWinner :: pokemon -> foe -> Winner pokemon foe
>
>   battle :: (Pokemon pokemon, Pokemon foe) => pokemon -> foe -> IO ()
>   battle pokemon foe =
>     printBattle
>       (show pokemon) (show move) (show foe) (show foeMove) (show winner)
>     where
>       move = pickMove pokemon
>       foeMove = pickMove foe
>       winner = pickWinner pokemon foe
>
> instance Battle Water Fire where
>   pickWinner pokemon _ = pokemon
>
> instance Battle Fire Water where
>   type Winner Fire Water = Water
>   pickWinner _ foe = foe
>
> instance Battle Grass Water where
>   pickWinner pokemon _ = pokemon
>
> instance Battle Water Grass where
>   type Winner Water Grass = Grass
>   pickWinner _ foe = foe
>
> instance Battle Fire Grass where
>   pickWinner pokemon _ = pokemon
>
> instance Battle Grass Fire where
>   type Winner Grass Fire = Fire
>   pickWinner _ foe = foe
>
> main :: IO ()
> main =
>   battle Squirtle Charmander
