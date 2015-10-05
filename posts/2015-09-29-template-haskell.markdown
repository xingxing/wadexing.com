---
title: Template Haskell
---

[原文](https://ocharles.org.uk/blog/guest-posts/2014-12-22-template-haskell.html)


Template Haskell是定义在Haskell 98中的一个扩展，目的是：**编译时元编程**。即Haskell语法和位于其下的GHC抽象语法数之间的直接转换。
任何熟知Lisp宏系统的人都会立即发觉它们太相似了，虽然是在Haskell中，用一些特定的数据类型来表示AST，从而描述和焊接成代码片段。
在编译时生成代码的能力让我们可以实现类似宏的扩展，多态程序，用户定制优化(如代码嵌入)，并从现有的数据结构和函数中生成辅助性的数据结构和函数。

简单来说，牛津括号[|和|]用来获取抽象语法树，拼接括号 $(和)用来将AST还原成Haskell代码。 Quotation Monad用来


In brief, Oxford brackets [| and |] are used to get the abstract syntax tree for the enclosed expression and ‘splice’ brackets $( and ) are used to convert from the abstract syntax tree back into Haskell. The Quotation Monad is used to give unique names to the parsed tokens from the supplied Haskell code, and reification can be used to look up the name, type, constructor, and state of expression, and as well as the AST of Haskell types.2
