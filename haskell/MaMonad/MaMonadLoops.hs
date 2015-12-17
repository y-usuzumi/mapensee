import Control.Monad
import Control.Monad.Loops
import Control.Monad.ST
import Data.STRef


main :: IO ()
main = do
  x <- runST $ do
      a <- newSTRef 0;
      curra <- readSTRef a;
      writeSTRef a (curra + 1);
      return curra
    `untilM` do
        curra <- readSTRef a
        return $ curra == 3
  return ()
