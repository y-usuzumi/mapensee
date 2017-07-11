{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

import Language.Haskell.TH
import Str (str)

fuck :: String
fuck = [str|sdf|]

main :: IO ()
main = do
  let x = fuck
  putStrLn x
