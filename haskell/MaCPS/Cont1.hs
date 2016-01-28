{-
论为什么笨人不适合学习Haskell...

在任何时候，智商都可以转换为生产力。其他条件相同的情况下，智商越高，生产力越旺盛。。。
所以真正让Haskell独树一帜的并非它的简洁，而是使用Haskell的人。。。

我，虽然无限接近于傻逼，但还是有尊严的。起码在别人面前尽量表现得不那么傻逼。。。

Haskell的CPS让我意识到自己到傻逼的距离从∞更接近到∞^∞。但为了尊严，必须奋战到天明。。。

(https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style)

-}

-- 非CPS风格的pythagoras

add :: Int -> Int -> Int
add x y = x + y

square :: Int -> Int
square x = x * x

pythagoras :: Int -> Int -> Int
pythagoras x y = square x `add` square y

-- CPS风格的pythagoras

{-
什么叫CPS呢？ 就是他妈该返回结果的时候不返回结果，等着别人传一个函数再把结果扔给它。是不是挺二逼的？
-}

add_cps :: Int -> Int -> (Int -> Int) -> Int
add_cps x y = \fuck -> fuck $ add x y

square_cps :: Int -> (Int -> Int) -> Int
square_cps x = \fuck -> fuck $ square x

pythagoras :: Int -> Int -> (Int -> Int) -> Int
pythagoras x y = \fuck ->
  square_cps x $ \x_squared ->
  square_cps y $ \y_squared ->
  add_cps x_squared y_squared fuck


{-
我恨你世界！
-}

main :: IO ()
main = do
  return ()
  return ()  -- 为什么要写两个呢？因为要让flycheck别他妈的警告了
