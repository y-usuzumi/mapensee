import Control.Monad
import Text.Printf

f :: (Integer, Integer) -> (Integer, Integer)
f (export, curr) =
  let (q, r) = curr `quotRem` 3
      ret = (export * 10 + q, r*10+q)
  in if r == 0 && q == 6 then ret else f ret

main :: IO ()
main = do
  let result = fst $ f (2, 2)
      -- Check
      s = show result
      rotated = (last s : init s)
      (q, r) = read rotated `quotRem` result
  when (q /= 3) $ putStrLn "Error"
  when (r /= 0) $ putStrLn "Error"
  print result
