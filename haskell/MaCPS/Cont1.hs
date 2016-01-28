{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-
论为什么笨人不适合学习Haskell...

在任何时候，智商都可以转换为生产力。其他条件相同的情况下，智商越高，生产力越旺盛。。。
所以真正让Haskell独树一帜的并非它的简洁，而是使用Haskell的人。。。

我，虽然无限接近于傻逼，但还是有尊严的。起码在别人面前尽量表现得不那么傻逼。。。

Haskell的CPS让我意识到自己到傻逼的距离从∞更接近到∞^∞。但为了尊严，必须奋战到天明。。。

(https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style)

-}

import Control.Monad

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

addCPS :: Int -> Int -> (Int -> Int) -> Int
addCPS x y = \fuck -> fuck $ add x y

squareCPS :: Int -> (Int -> Int) -> Int
squareCPS x = \fuck -> fuck $ square x

pythagorasCPS :: Int -> Int -> (Int -> Int) -> Int
pythagorasCPS x y = \fuck ->
  squareCPS x $ \x_squared ->
  squareCPS y $ \y_squared ->
  addCPS x_squared y_squared fuck

-- 非CPS风格的thrice
-- 如此简单

thrice :: (a -> a) -> a -> a
thrice f = f . f . f

-- CPS风格的thrice
-- 非要把简单的问题复杂化

thriceCPS :: (a -> (a -> r) -> r) -> a -> (a -> r) -> r
thriceCPS f a = \fuck ->
  f a $ \a_fucked ->  -- 每一个CPS函数的最后一个参数是一个函数，这个函数的参数是CPS包装的函数的执行结果（此处a_fucked相当于非CPS版的f a
  f a_fucked $ \a_fucked2 -> -- 此处的a_fucked2相当于非CPS版的f (f a)
  f a_fucked2 fuck


{-
接下来是第一个坑，从这里我就开始意识到智商充值的必要性

chainCPS :: ((a -> r) -> r) -> (a -> (b -> r) -> r) -> (b -> r) -> r

给你类型，让你实现。。

我们一步步分析一下：

chainCPS要长得像这个样子：

chainCPS fx fy = \fuck -> ...

fy要一个a，为了拿到a，必须传一个函数给fx：
chainCPS fx fy = \fuck ->
  fx $ \a -> ...

好fy可以用了：
chainCPS fx fy = \fuck ->
  fx $ \a -> fy a

还差一点，fx接收的函数要返回一个r，那么仅差一步：
chainCPS fx fy = \fuck ->
  fx $ \a -> fy a fuck

大功告成！
但是为什么觉得这么空虚呢？
没错，我们似乎纯是为了拼类型而写代码。这个chainCPS到底是怎样的逻辑呢？

刚才说过，CPS的函数参数接收的是CPS包装的函数的执行结果，因此，这里的a就是
fx包装的函数的结果（但是呢，在上面的例子中，我们直接把结果应用到函数参数
便结束了，如\fuck -> fuck $ add x y，并未再做其他的事情，而实际上
CPS可以将结果再进一步处理，如\fuck -> fuck (add x y) * 2。。
但是不管怎么说，这里的a便是fx中直接应用到函数参数上的值，之后将该值应用到
fy上，得到一个新的CPS，将一个新的函数应用到这个CPS上得到的结果再反过来
由fy及fx处理。数据流大概是这样：
                  fuck
   |---------       |
   v        |       v
fx中的值x -> fy -> fy a
            ^       |
            |-------

图画得不知所云_(:з」∠)_

重新定义一下pythagorasCPS：

pythagoras :: (Int -> Int) -> Int
pythagoras = chainCPS (squareCPS 3) (\x -> chainCPS (squareCPS 4) (\y -> addCPS x y))

-}

chainCPS :: ((a -> r) -> r) -> (a -> (b -> r) -> r) -> (b -> r) -> r
chainCPS fx fy = \fuck ->
  fx $ \a -> fy a fuck

{-
看到这里有没有熟悉的影子？

卧槽这个(a -> r) -> r不应该是某个单子实例么？？！！
卧槽这个chainCPS不就是(>>=)么？？！！

于是我们的Cont monad走起！！！
instance Monad ((-> r) -> r)
...

等等，这什么鬼？
早在学Monad第一课的时候就告诉你了，函数的monad实例是如何定义的？
instance Monad ((->) r)
而不是
instance Monad (r ->)
要问为啥。。。
一个data FuckinData a b，你能写
instance Monad (FuckinData * b)么？显然不行。。。
所以此路不通。。。

大概还需要定义一个类型。。。
-}

newtype Cont r a = Cont { runCont :: ((a -> r) -> r) } deriving Functor

{-
当然啦，标准库里没有导出Cont构造函数，所以：
-}

cont :: ((a -> r) -> r) -> Cont r a
cont = Cont

{-
液！
Monad实例信手拈来（。。。吧）
-}

instance Applicative (Cont r) where
  pure = return
  (<*>) = ap

instance Monad (Cont r) where
  return a = cont $ \f -> f a
  l >>= f = cont $ \k ->
    runCont l $ \a -> runCont (f a) k


{-
我恨你世界！
-}

main :: IO ()
main = do
  return ()
  return ()  -- 为什么要写两个呢？因为要让flycheck别他妈的警告了
