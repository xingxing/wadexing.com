---
title: Overloaded Strings
---


[原文](https://ocharles.org.uk/blog/posts/2014-12-17-overloaded-strings.html)

今天我们将介绍一个实用的扩展，它使我们可以重新定义Haskell中字面量的意义。通常，String字面量有个具体类型:

``` haskell
.> :t 42
42 :: Num a => a
```

之后，我们写一个基于Num类型类的多态实例。同样的，一个Float小数字面量

``` haskell
.> :t 3.142
3.142 :: Fractional a => a
```

这是一个基于Fractional类型类的多态实例。

这个多态威力强大，它允许我们在Haskell代码写内嵌的DSL，而无需引入新的结构(普通的值不算)。那String为什么这样不同呢？还好String不是必须与众不同。如果我们应用overloaded string扩展，String字面量会获得一个不同的类型:

``` haskell
.> :set -XOverloadedStrings
.> :t "Oh, what's this?"
"Oh, what's this?" :: Data.String.IsString a => a
```

通过应用这一扩展，String字面量现在通过fromString函数调用生成，并是IsString类型类的一个实例。
你将可以在Haskell中找到一些基于此类型类的实例，这也使得此扩展成为程序员们工作中最常用的一个。一些明显的实例，如IsString Text，但也有些有意思的用法。
其中一个是[postgresql-simple](http://hackage.haskell.org/package/postgresql-simple)库中的用法。在这个库中，我们可以通过写SQL查询来和PostgreSQL关系型数据库交互。然而，连接字符串生成的SQL查询因注入攻击而声名狼藉，postgresql-simpel提供一个Query类型只是IsString的实例。这样就使得写字面量查询非常轻量，但当我们想连接字符串来构成查询的时候，我们必须非常明确。这是一个很非入侵式的功能，但我觉得它可以很好地提醒开发者风险随时到来。
