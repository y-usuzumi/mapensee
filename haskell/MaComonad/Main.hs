import U

main :: IO ()
main = do
  let u = U [-2, -3] (-1) [0, 1, 2, 3]
  print (iterate left u)
  return ()
