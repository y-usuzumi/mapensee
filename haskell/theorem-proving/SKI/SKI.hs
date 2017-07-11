{-# LANGUAGE GADTs #-}

module SKI where

data T t where
  S :: T ((a -> b -> c) -> (a -> b) -> a -> c)
  K :: T (a -> b -> a)
  I :: T (a -> a)
  (:$) :: T (a -> b) -> T a -> T b

instance Show (T t) where
  show S = "s"
  show K = "k"
  show I = "i"
  show (a :$ b) = show a ++ "(" ++ show b ++ ")"


type TChurch a = T ((a -> a) -> (a -> a))

encodeCharT :: Char -> TChurch a
encodeCharT '\n' = K :$ I
encodeCharT c = let
  p = encodeCharT (pred c)
  in
  S
  :$ ( S
       :$ (K :$ S)
       :$ (S :$ (K :$ K) :$ I)
     )
  :$ ( S
       :$ (S :$ (K :$ S) :$ (S :$ (K :$ K) :$ (S :$ (K :$ p) :$ I)))
       :$ (K :$ I)
     )
