{-# LANGUAGE QuasiQuotes #-}

module Util where

import Language.Haskell.TH.Quote

str :: QuasiQuoter
str = QuasiQuoter { quoteExp = return }
