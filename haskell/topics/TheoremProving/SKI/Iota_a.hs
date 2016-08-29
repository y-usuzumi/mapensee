-- Does not typecheck

s :: (a -> b -> c) -> (a -> b) -> a -> c
s a b c = a c (b c)

k :: a -> b -> a
k = const

i :: a -> a
i = id

u :: (((a -> b -> c) -> (a -> b) -> a -> c) -> (d -> e -> d) -> f) -> ((a -> b -> c) -> (a -> b) -> a -> c) -> (d -> e -> d) -> f
u f = f s k

a = u(u(u(u)))(u(u))
-- a = u(u(u(u(u))))(u(u(u(u(u))))(u(u(u(u)))(u(u(u(u(u))))))(u(u(u(u(u))))(u(u(u(u)))(u(u(u(u)))))(u(u))))(u(u(u(u(u))))(u(u(u(u(u))))(u(u(u(u)))(u(u(u(u(u))))))(u(u(u(u(u))))(u(u(u(u)))(u(u(u(u)))))(u(u(u(u(u))))(u(u(u(u)))(u(u(u(u(u))))(u(u(u(u(u))))(u(u(u(u)))(u(u(u(u(u))))))(u(u(u(u(u))))(u(u(u(u)))(u(u(u(u)))))(u(u))))(u(u(u(u(u))))(u(u(u(u(u))))(u(u(u(u)))(u(u(u(u(u))))))(u(u(u(u(u))))(u(u(u(u)))(u(u(u(u)))))(u(u(u(u(u))))(u(u(u(u)))(u(u(u(u(u))))(u(u(u(u(u))))(u(u(u(u)))(u(u(u(u(u))))))(u(u(u(u(u))))(u(u(u(u)))(u(u(u(u)))))(u(u))))(u(u(u(u(u))))(u(u(u(u(u))))(u(u(u(u)))(u(u(u(u(u))))))(u(u(u(u(u))))(u(u(u(u)))(u(u(u(u)))))(u(u(u(u(u))))(u(u(u(u)))(u(u(u(u)))(u(u))))(u(u)))))(u(u(u(u)))(u(u))))))(u(u)))))(u(u(u(u)))(u(u))))))(u(u)))))(u(u(u(u)))(u(u))))

main = print $ a succ '\n'
