module Data.Fix where

import           Data.Data
import           Data.Typeable
import           GHC.Generics

-- | A fix-point type.
newtype Fix f = Fix { unFix :: f (Fix f) } deriving (Generic, Typeable)
deriving instance (Typeable f, Data (f (Fix f))) => Data (Fix f)
