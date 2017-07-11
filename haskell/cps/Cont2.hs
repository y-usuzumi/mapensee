module Cont2 where

{-
callCC。。。
-}

import Control.Monad
import Control.Monad.Cont

default (Integer)

{-
不止一次卡在这个地方了。就如同当年卡在Monad处一样，每次看callCC都会倒吸一口凉气。光这个类型就足够吓退我了：

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a

这次我们先看几个例子，之后再回过头来解释这个类型。。
-}

foo :: Int -> Cont r String
foo x = callCC $ \k -> do
  let y = x ^ 2 + 3
  when (y > 20) $ k "over twenty"
  return (show $ y - 4)

{-
foo的逻辑一目了解，这里调用k的结果十分类似于其他命令式语言中的return，而结果将传入调用foo时传入的continuation中。

再来一个高级一点的例子：
-}

bar :: Char -> String -> Cont r Int
bar c s = do
  msg <- callCC $ \k -> do
    let s0 = c:s
    when (s0 == "hello") $ k "They say hello."
    let s1 = show s0
    return ("They appear to be saying " ++ s1)
  return (length msg)

{-
只需要记住，使用k返回后，后面的代码不会执行就行了。
-}

quux ::  Cont r Int
quux = callCC $ \k -> do
  let n = 5
  _ <- k n
  return 25 -- Won't reach here

{-
这时候我们再回过头来看callCC的类型：

callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a

首先，我们可以肯定的是，在callCC的参数（上面的lambda \k -> ...）中，返回值类型一定和整个callCC的返回值类型相同（Cont r a)。

在这个前提下，我试试能不能直接写出callCC的定义出来：

...想了一分钟，放弃了。。

结果是：

callCC f = cont $ \h -> runCont (f (\a -> cont $ \_ -> h a)) h

-}

{-
不再纠结了。还有几个例子，看完了相当于Cont部分就结束了。。
-}
