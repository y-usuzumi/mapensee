Expression Quotation
====================

如下几种：
[e|...|] 或 [|...|]：此为 Q Exp 类型
[d|...|]：此为 Q [Dec] 类型
[t|...|]：此为 Q [Type] 类型
[p|...|]：此为 Q [Pat] 类型


Splice
======

如下形式：
$x（x为标识符）或 $(...)（任意表达式）

代替以下场合：
表达式：此时...为Q Exp类型
一个/若干声明：此时...为Q [Dec]类型
类型：此时...为Q Type类型
模式：此时...为Q Pat类型


对于Quotation和Splice，分别有带类型的版本：
[||...||] / [e||...||] 和 $$x/$$(...)
其中...的类型是a，则Quotation的类型是Q (TExp a)

转换： unType :: TExp a -> Exp


Quasi-Quotation
===============

如下形式：
[quoter|...|]
其中：
...为任意字符串，可以包含换行
quoter由其他模块导入的(unqualified) name
quoter不能是e, d, t, p
[quoter| 这部分不能包含空格
遇到|]即结束，如果想在...中包含|]，你需要自己设计转义规则

Quasiquote仍然可以代替上面四种场合。

所谓quoter是一个Language.Haskell.TH.Quote.QuasiQuoter类型值，有如下定义：

> data QuasiQuoter = QuasiQuoter { quoteExp  :: String -> Q Exp,
>                                  quotePat  :: String -> Q Pat,
>                                  quoteType :: String -> Q Type,
>                                  quoteDec  :: String -> Q [Dec] }

