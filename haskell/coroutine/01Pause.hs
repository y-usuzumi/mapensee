data Pause m = Run (m (Pause m))
             | Done

runN :: Monad m => Int -> Pause m -> m (Pause m)
runN _ Done = return Done
runN 0 p = return p
runN n (Run r) = r >>= runN (n-1)
