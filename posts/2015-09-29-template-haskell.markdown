---
title: Template Haskell
---

[原文](https://ocharles.org.uk/blog/guest-posts/2014-12-22-template-haskell.html)


Template Haskell是定义在Haskell 98中的一个扩展，目的是：**编译时元编程**。即Haskell语法和位于其下的GHC抽象语法数之间的直接转换。
任何熟知Lisp宏系统的人都会立即发觉它们太相似了，虽然是在Haskell中，用一些特定的数据类型来表示AST，从而描述和焊接成代码片段。
在编译时生成代码的能力让我们可以实现类似宏的扩展，多态程序，用户定制优化(如代码嵌入)，并从现有的数据结构和函数中生成辅助性的数据结构和函数。

简单来说，牛津括号[|和|]用来获取抽象语法树，拼接括号 $(和)用来将AST还原回Haskell代码。
Quotation Monad用以提供唯一性的，这些名称用来支持Haskell代码，reification(具象化)被用于查找名称、类型、结构及表达式的状态，也AST类型也在其内。

Template Haskell 是由Tim Sheard 和 Simon Peyton Jones 在他们2002年的论文《Template Meta-Programming for Haskell》中引入的，尽管此后有了些许变化[参考](http://research.microsoft.com/en-us/um/people/simonpj/tmp/notes2.ps)。
它继承自C++ templated，尽管TH是函数式的与宏系统更为类似。Quasiquotation常与Template Haskell搭配使用，但篇幅有限，此处将略陈其意。因为quasiquotation确实值得另写一章。

在实践中，TH被广泛应用于Yesod的路由和HTML模板绑定。在Haskell之外，编译时元编程被用于创建DSL(特定领域语言)，典型的领域有测试，建模，对象关系映射的一般元编程(无论是否是编译时)：如数据库结构映射为非编译代码。在Lisp界声名大噪的宏系统中，元编程被用以创建扩展语法（语法糖），如用于lisp comprehensions的语法。

-------

本文中代码均在GHCi version 7.6.3 和 Template Haskell version 2.9.0.0中测试执行过。

作为起手式，启动包含 Template Haskell 扩展的 GHCI，之后加载AST数据类型。

``` haskell
$ ghci -XTemplateHaskell
Prelude> :m + Language.Haskell.TH
Prelude Language.Haskell.TH>
```

为了看到Haskell代码的AST语法，我们将可用的Haskell代码写入牛津括号并通过runQ来执行，runQ基于Q monad (quotation monad)

``` haskell
> runQ [| 1 + 2 |]
InfixE (Just (LitE (IntegerL 1))) (VarE GHC.Num.+) (Just (LitE (IntegerL 2)))
```

透过这些圆括号，你将看到返回表达式是一个树型 一棵抽象语法树! 

<img src="https://raw.githubusercontent.com/seanwestfall/templatehaskell/master/syntax_tree.png" alt="abstract syntax tree">

查一下通过牛津括号调用的Lift类[源码](http://hackage.haskell.org/package/template-haskell-2.7.0.0/docs/src/Language-Haskell-TH-Syntax.html#Lift)。Language.Haskell.TH.Syntax模块包含了所有在AST中用到的类型的定义。应用这些类型，
它可以构成任意的Haskell代码片段。来看一下[Lit](http://hackage.haskell.org/package/template-haskell-2.9.0.0/docs/Language-Haskell-TH-Syntax.html#t:Lit)数据类型，例如基于字面量的Lit。

``` haskell
data Lit = CharL Char
     | StringL String
     | IntegerL Integer     -- ^ 用于重载和非重载的 
                            --   字面量。我们此刻尚未找
                            --   到表示非重载字面量的妙法
                            --   也许这并无妨碍？
     | RationalL Rational   --   同上
     | IntPrimL Integer
     | WordPrimL Integer
     | FloatPrimL Rational
     | DoublePrimL Rational
    deriving( Show, Eq, Data, Typeable )
````

其组成的字面量定于遍布在的你的AST语法中，参看前面例子中的AST可知。在Language.Haskell.TH.syntax中，35种通用数据类型被声明在[Data.Data](http://hackage.haskell.org/package/base-4.6.0.1/docs/Data-Data.html)模块中。
如对AST语法好奇可参看其[源码](http://hackage.haskell.org/package/template-haskell-2.7.0.0/docs/src/Language-Haskell-TH-Syntax.html#line-716)。

[Q](http://hackage.haskell.org/package/template-haskell-2.9.0.0/docs/Language-Haskell-TH-Syntax.html#t:Q) monad通过上下文控制表达式的类型，同时提供在控制范围内唯一的名字：表达式名后跟一个长整数。Quotation 族是词法作用域， Q monad 用它的命名格式来控制它。(对于TH的词法作用域更深层的解释请见 [wiki](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/template-haskell.html))

让我们来绑定前面例子中返回的那个AST表达式，生成成Haskell代码并对其求值，用拼接符号:

```` haskell
> $( return (InfixE (Just (LitE (IntegerL 1))) (VarE (mkName "+")) (Just (LitE (IntegerL 2)))))

3
````

哒哒！你将正确的Haskell转成了AST并且对其求值。尽管，诚如你所见，AST中的符号(例子中的+)必须用mkName类型来定义才可以正确地求值。
这可以尽可能避免为拼接回去而必须修改AST,但必须将其绑定为变量，正如我们下一个例子中展示的那样：

此例中，斐波那契数列将由zipWith生成：

```` haskell
let fibs :: [Integer]
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

let fibQ :: Int -> Q Exp
    fibQ n = [| fibs !! n |]
````

现在运行 $( ... ) 来执行这个表达式:

```` haskell
> $(fibQ 22)
17711
````

TH 将fibQ连同变量一同拼接成一个表达式(此处是 fib !! n)。

请注意，表达式和拼接是可以嵌套的。

```` haskell
> $(runQ [| fibs !! $( [| 8 |]) |])
21
````

下面我会解释TH的语法,之后，我会展示令人难忘的示例以展现拼接和ASTs的威力。

### 语法

Template Haskell 引用表达式由4个不同的分析器类型，及第五个可选的泛类型，以便大家定义自己的引用类型，也被成为准-引用(quasi-quotations)。

- [| ... |] 或 [e| ... |] 生成表达式的AST语法；其类型为 Q Exp

```` haskell
> runQ [| 1 + 2 |]
InfixE (Just (LitE (IntegerL 1))) (VarE GHC.Num.+) (Just (LitE (IntegerL 2)))
````

- [d| ... |] 生成一个**顶层声明**的AST语法列表; 其类型为 Q [Dec]

```` haskell
> runQ [d|x = 5|]
[ValD (VarP x_4) (NormalB (LitE (IntegerL 5))) []]
````

- [t| ... |], 生成一个类型的AST语法；其类型为 Q Type

```` haskell
> runQ [t|Int|]
ConT GHC.Types.Int
````

- [p| ... |], 生成一个元组的AST语法；其类型为 Q Pat

```` haskell
> runQ [p|(x,y)|]
TupP [VarP x_5,VarP y_6]
````

- 定制的准-引用, 形如["引用名"|...|]。引用名可以是任意标示但不能是e, d, t 也不可包含空格的。尽管，所有GHC都会基于上下文决定使用哪个分析器。

  准-引用对元编程来说很有帮助。他们本质上就是在创造DSLs。至此本文已经颇长，不宜赘述，如果看官对此主题感兴趣，可参看quasi-quotations的[指南1](https://www.cs.drexel.edu/~mainland/publications/mainland07quasiquoting.pdf), [2](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/template-haskell.html#th-quasiquotation)及[3](https://www.fpcomplete.com/user/marcin/quasiquotation-101)


Template Haskell中有个重要的限制要记牢，即在拼接中只能调用已经导入的模块中定义的函数，同一模块的其他地方没有函数定义。引用和拼接必须被定义在独立的模块中，若非如此将报错：

```` shell
GHC stage restriction:
`...' is used in a top-level splice or annotation,
and must be imported, not defined locally
````

尽管，如果你正在GHCi中用let绑定变量，无须担心。只有在编译的时候必须注意。

### 调试和具象化(Reification)

如果你能用另一种方式对一个Q表达式进行求值，来观察对拼接的求值，你会不会很惊讶？当然可以，执行runQ( Q Exp ) >>= putStrLn.pprint 来观察一个Q Exp类型的表达式将会被求值为：

```` haskell
> let myExp :: Q Exp; myExp = runQ [| 1 + 2 |]
> runQ(myExp) >>= putStrLn.pprint
1 GHC.Num.+ 2
````

如果你想看到拼接的表达式，用参数-ddump-splices来运行GCHi: ghci -XTemplateHaskell -ddump-splices

现在就用一个有趣的素数的例子来试一下吧

```` haskell

let isPrime :: (Integral a) => a -> Bool
    isPrime k | k <=1 = False | otherwise = not $ elem 0 (map (mod k)[2..k-1])

let nextPrime :: (Integral a) => a -> a
    nextPrime n | isPrime n = n | otherwise = nextPrime (n+1)

-- returns a list of all primes between n and m, using the nextPrime function
let doPrime :: (Integral a) => a -> a -> [a]
    doPrime n m
         | curr > m = []
         | otherwise = curr:doPrime (curr+1) m
        where curr = nextPrime n

-- and our Q expression
let primeQ :: Int -> Int -> Q Exp
    primeQ n m = [| doPrime n m |]
````

```` haskell
> $(primeQ 0 67)

<interactive>:18:3-13: Splicing expression
primeQ 0 67 ======> doPrime 0 67
[2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67]
````

在嵌套表达式里试一下下，看到dump-splices的真正作用

```` haskell
> $(primeQ ($(primeQ 0 23) !! 3) 167)

<interactive>:20:13-23: Splicing expression
primeQ 0 23 ======> doPrime 0 23
<interactive>:20:3-34: Splicing expression
primeQ ($(primeQ 0 23) !! 3) 167 ======> doPrime 7 167
[7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167]
````

-ddump-splices 和 >>= putStrLn.pprint 让调试简单许多。

接下来可能是至少我认为是Template Haskell最难理解的部分**具象化**

具象化允许查询一个Haskell [Name](http://hackage.haskell.org/package/template-haskell-2.9.0.0/docs/Language-Haskell-TH-Syntax.html#t:Name) 的状态和它们的信息。特别是，具体化地返回一个成为info的数据类型，这个数据类型基于任意的名称及特定的格式，info的格式取决于其是在 __类型上下文__ 还是 __表达式上下文__ 中被解释的。

TH 特别为具象化引入了两个新标示符：Prefix Names 用来被求值一个单个引号的表达式上下文， prefix Names 用来被求值一个*双引(2个')*的类型上下文。尽管，Names 必须是这些被具象化的上下文中可解释的。（如果你想在表达式中使用具象化，那就别在这个些表达式的的名字里使用引号-除非不想它被正确解析）

用 **双引** 来具象化一个类型：

```` haskell
> $(stringE . show =<< reify ''Bool)
"TyConI (DataD [] GHC.Types.Bool [] [NormalC GHC.Types.False [],NormalC GHC.Types.True []] [])"
````

具象化一个类型返回AST, 下图示意了布尔类型

<img src="https://raw.githubusercontent.com/seanwestfall/templatehaskell/master/syntax_tree_bool.png" alt="abstract syntax tree boolean">


简单数据类型如布尔型的AST产生一个小的树，但当应用在模块链更深的类型上的时候，相当于大量ASTs将会被生成。试试具象化 ''Lit 或者 ''Exp，你就知道鄙人所言不虚，尽管具象化能够在所有的Haskell类型上施用。


用 **单个引号** 来具象化一个表达式，下面这个例子用到了我们的素数表达式：

```` haskell
> $(stringE . show =<< reify 'primeQ)
"VarI primeQ_1627395913 (AppT (AppT ArrowT (ConT GHC.Types.Int)) (AppT (AppT ArrowT (ConT GHC.Types.Int)) (AppT (ConT Language.Haskell.TH.Syntax.Q) (ConT Language.Haskell.TH.Syntax.Exp)))) Nothing (Fixity 9 InfixL)"
````

如你所见，info 返回了不同的信息取决于其是一个类型还是一个表达式。一个类型返回其在TH的AST语义中的结构。一个表达式返回它的name，类型，其结构，及其固定性规则(fixity, 优先级和结合性规则的组合通常称之为固定性规则)。

使用表达式的具象化获取一个表达式的结构及相关的类型，之后具象化这些类型来得到它的AST结构。这允许我们在Haskell中生成任意数据类型的AST，无论其在Haskell中的深度如何。

具象化从绘制AST到在编程语言内部拼接还原代码片段的立场上看， 是极其有用的。下面的第二个例子中，将展示如何使用具象化来从record结构中获取类型，生成一个可以在任何record上使用的Show函数。

### 例子

一个好例子：类型安全的C语言printf函数的Haskell实现(来自 stdion.h)

*Main.hs*

```` haskell

{-# LANGUAGE TemplateHaskell #-}

-- 导入我们的模板 "printf"
import PrintF (printf)

-- 拼接操作符 $ 于编译时通过"printf"生成Haskell
-- 代码拼接成putStrLn的参数
main = do
   putStrLn $ $(printf "Hello %s %%x%% %d %%x%%") "World" 12
   putStrLn $ $(printf "Hello %s %s %s %d") "Russian" "with" "Love" 5000

````

*PrintF.hs*

```` haskell
{-# LANGUAGE TemplateHaskell #-}
module PrintF where

-- NB: printf needs to be in a separate module to the one where
-- you intend to use it.

-- Import some Template Haskell syntax
import Language.Haskell.TH

-- Possible string tokens: %d %s and literal strings
data Format = D | S | L String
     deriving Show

-- a poor man's tokenizer
tokenize :: String -> [Format]
tokenize [] = []
tokenize ('%':c:rest) | c == 'd' = D : tokenize rest
                      | c == 's' = S : tokenize rest
tokenize (s:str) = L (s:p) : tokenize rest -- so we don't get stuck on weird '%'
     where (p,rest) = span (/= '%') str

-- generate argument list for the function
args :: [Format] -> [PatQ]
args fmt = concatMap (\(f,n) -> case f of
                                    L _ -> []
                                    _   -> [varP n]) $ zip fmt names
          where names = [ mkName $ 'x' : show i | i <- [0..] ]

-- generate body of the function
body :: [Format] -> ExpQ
body fmt = foldr (\ e e' -> infixApp e [| (++) |] e') (last exps) (init exps)
     where exps = [ case f of
                    L s -> stringE s
                    D   -> appE [| show |] (varE n)
                    S   -> varE n
                    | (f,n) <- zip fmt names ]
     names = [ mkName $ 'x' : show i | i <- [0..] ]

-- glue the argument list and body together into a lambda
-- this is what gets spliced into the haskell code at the call
-- site of "printf"
printf :: String -> Q Exp
printf format = lamE (args fmt) (body fmt)
    where fmt = tokenize format
````

注意我们必须将拼接和表达式定义在独立的模块中定义，正如前面提到过的。

编译命令如下：

```` shell
$ ghc --make Main.hs -o main
````

执行main输出如下：

```` shell
$ ./main
Hello World %%x%% 22 %%x%%
Hello Russian with Love 5000
````

下面的例子展示具象化可以做什么 一个泛化Show可以为任意record产生Show函数：

*Main.hs*

```` haskell
{- Main.hs -}
module Main where
import Derive

data T = A Int String | B Integer | C
$(deriveShow ''T)

main = print [A 1 "s", B 2, C]  -- prints exactly <<[A 1 "s",B 2,C]>>
````

*Derive.hs*

```` haskell
{- Derive.hs -}
module Derive where

import Language.Haskell.TH
import Control.Monad

data T1 = T1
data T2 a = T2 a

deriveShow t = do
-- Get list of constructors for type t
TyConI (DataD _ _ _ constructors _)  <-  reify t

-- Make `show` clause for one constructor:
--   show (A x1 x2) = "A "++show x1++" "++show x2
let showClause (NormalC name fields) = do
-- Name of constructor, i.e. "A". Will become string literal in generated code
let constructorName = nameBase name
-- Get variables for left and right side of function definition
(pats,vars) <- genPE (length fields)
-- Recursively build (" "++show x1++...++"") expression from [x1...] variables list
let f []       = [| "" |]
f (v:vars) = [| " " ++ show $v ++ $(f vars) |]
-- Generate function clause for one constructor
clause [conP name pats]                                 -- (A x1 x2)
(normalB [| constructorName ++ $(f vars) |]) []  -- "A "++show x1++" "++show x2

-- Make body for function `show`:
--   show (A x1 x2) = "A "++show x1++" "++show x2
--   show (B x1)    = "B "++show x1
--   show C         = "C"
showbody <- mapM showClause constructors

-- Generate template instance declaration and then replace
--   type name (T1) and function body (\x -> "text") with our data
d <- [d| instance Show T1 where
show x = "text"
|]
let    [InstanceD [] (AppT showt (ConT _T1)) [FunD showf _text]] = d
return [InstanceD [] (AppT showt (ConT t  )) [FunD showf showbody]]


-- Generate n unique variables and return them in form of patterns and expressions
genPE n = do
ids <- replicateM n (newName "x")
  return (map varP ids, map varE ids)
````

编译命令如下：

```` shell
$ ghc --make Main.hs -o main
````

执行main输出如下：

```` shell
$ ./main
[A 1 "s",B 2,C]
````

### 总结

This guide was for the most part written from collecting information written in other guides on Template Haskell, quasi-quoting, and Lisp macros – from online, wiki, and academic sources. Please check my bibliography to see where what came from what so credit can be properly given where it’s due.

Meta-programming is a powerful programming technique that can allow for the generation of user generated syntax extensions and DSLs. This is useful in that it can allow a programmer to generate custom code generating syntax extensions without otherwise having to change the core language. Template Haskell in particular is especially powerful over similar programming language constructs (i.e. The C Preprocessor, Lisp’s Macro system) in that it makes use of ASTs, reification (through a specific function), and – much in the spirit of Haskell – type-safety. The examples presented above only scratch the surface of what’s possible with reification – imagine the ability to construction entire systems, and then use reify to build ASTs, then swap in and out entire modules, entirely with the use of Template Haskell.

Some questions that have arisen within me from writing this article are: What are the limits of TH’s data type system? Is it truly possible for TH to represent all of Haskell with the finite set of data types written into the module? Is it possible for future language features to defy this set? What are the limits of meta-programming – TH, macros, and similar meta-prorgramming constructs make it possible to write code that writes code – but are there limits to this – is it possible to write a macro that can generate a macro, and so on indefinitely?

Don’t forget to checkout the API. Everything you need to know, you can for the most part find in the source. Also TH does in fact have bugs, check the issue tracking page if you’re dealing with a known issue: see here.
