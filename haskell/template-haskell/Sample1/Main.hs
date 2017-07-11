{-# LANGUAGE TemplateHaskell #-}

import Fibs

main :: IO ()
main = do
  let f = $(fibQ 10)  -- $(..) splice进行bind和求值。
  print f
