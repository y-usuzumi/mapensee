{-# LANGUAGE TemplateHaskell #-}

module Fibs ( fibs
            , fibQ
            ) where

import Language.Haskell.TH

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibQ :: Int -> Q Exp
fibQ n = [e| fibs !! n |]  -- [|...|]等同于[e|...|]，返回一个Q Exp
