module MaFRP.Netwire.Test where

import Control.Wire
import Prelude hiding
       ((.)
       , id
       )

wire :: (Monad m, HasTime t s) => Wire s () m a t
wire = time

main :: IO ()
main = testWire clockSession_ wire
