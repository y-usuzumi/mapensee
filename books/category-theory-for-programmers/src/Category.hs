-- | module Category
-- Category接收两个对象类型a, b，具体化的Category实际是a -> b的态射。
--
-- Q: 为什么不用一个类型x封装所有的对象，如：
--   class Category cat where
--     type Object cat
--     type Morphism cat :: Object cat -> Object cat -> *
-- A: 目前发现有两个原因：
--    其一：上面定义GHC不支持，会报defined and used in the same recursive group错。
--    其二：这样失去了GHC的类型推断能力，关联不同对象的态射都是同一个类型。


class Category (cat :: k -> k -> *) where
  id :: cat a a
  (.) :: cat b c -> cat a b -> cat a c

