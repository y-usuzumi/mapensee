module Main

i : a -> a
i = id

k : a -> b -> a
k = const

s : (c -> b -> a) -> (c -> b) -> c -> a
s a b c = a c (b c)

kxkx'ix : k x (k x) = i x
kxkx'ix = Refl

skkx'kxkx : s k k x = k x (k x)
skkx'kxkx = Refl

skkx'ix : s k k x = k x (k x) -> k x (k x) = i x -> s k k x = i x
skkx'ix Refl Refl = Refl

skkx'ix' : s k k x = i x -> s k k = i
skkx'ix' (skkx'ix Refl Refl) = Refl

main : IO ()
main = putStrLn "OK"
