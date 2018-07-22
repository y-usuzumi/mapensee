{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeFamilyDependencies #-}

data V = V0 | V1 | V2

type family VNext a = b | b -> a where
  VNext V0 = V1
  VNext V1 = V2
  VNext V2 = V0

data TList a e where
  TEmpty :: TList V0 e
  TCons :: e -> TList a e -> TList (VNext a) e

v2Head :: TList V2 e -> e
v2Head (TCons e _) = e

v2Tail :: TList (VNext a) e -> TList a e
v2Tail (TCons _ t) = t

main :: IO ()
main = undefined
