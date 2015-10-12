----
title: lens 是什么鬼 (一)
----

[原文](http://blog.jakubarnold.cz/2014/07/14/lens-tutorial-introduction-part-1.html)

本文是即将呈现的一个系列的第一篇，剑指Haskell的lens库和其背后的理念，尽量通俗易通。要是你是个Haskell新手也别担心，唯一的先决条件是你得了解函子(Functor)类型类，并知道records和抽象数据类型怎么用。

我们在本文中还不涉及到lens库的使用。我们将以学习为目的从零开始重新实现一下lens，保持API的一致，从而想你展示这一些是如何做到的。

记住，lenses是Haskell中一个非常高级的主题，确实需要花点时间来消化。如果第一次阅读没彻底明白也别担心。

### lenses背后的动机

如果你是从命令式语言像Ruby或者Java转到Haskell上的，你很可能见惯了这样的代码：

```` ruby

project.owner.name = "John"

````

面向对象的信徒们可能会说这违反了德米忒耳原则，但是现下请忽略这个不良实践。这里的问题是，我们能否在Haskell做到这样？

```` haskell
data User = User { name :: String, age :: Int } deriving Show
data Project = Project { owner :: User } deriving Show

setOwnerName :: String -> Project -> Project
setOwnerName newName p = p { owner = (owner p) { name = newName } }
````

现在我们已经可以看到和理想有多大差距了。为了改变owner的name，我们需要在Project上为owner字段重新指派一个新的User，新的User通过记录语法更新的。我们可以用下面这些步骤做到。

λ> 代指GHCi会话中的

```` haskell
λ> let bob = User { name = "Bob", age = 30 }
λ> let project = Project { owner = bob }

λ> let alice = bob { name = "Alice" }
λ> let project2 = project { owner = alice }
````

这比之原来的ruby示例乏味得很，尤其是需要一直重建原始的更深层的结构体。

### 一个天真lens的实现

于是lenses来帮助你啦。本质上说，lenses只不过是一些可以组合getter和setter。一个幼稚的实现就像下面这样:

```` haskell
data NaiveLens s a = NaiveLens
{  view :: s -> a
 , set  :: a -> s -> s }
````

遵从官方[lens库](http://lens.github.io/)的约定，我们将类型参数命名为s和a：s为对象， a是焦点。由于我们正想改变项目用户的名字，在我们上面的例子中s就是User，而a应该是一个String。

现在给出一个NaiveLens User String类型的lens以便我们能够修改用户的名字。

```` haskell

λ> let john = User { name = "John", age = 30 }
λ> set nameLens "Bob" john
User {name = "Bob", age = 30}

````

这个lens是咋实现的？就是简单的setter和getter

```` haskell
nameLens :: NaiveLens User String
nameLens = NaiveLens name (\a s -> s { name = a })
````

这个实现的问题是数据类型中的getter和setter不易扩展。如果我们想要做为目标值加1的操作，我们就必须首先看到当前的值是多少，接着+1,然后在set新值。我们可以将这概况成第三个元函数over(覆盖)：

```` haskell
over :: (a -> a) -> s -> s
````

over的用法和set差不多

```` haskell
λ> let john = User { name = "John", age = 30 }
λ> over ageLens (+1) john
User {name = "John", age = 31}
````

```` haskell
ageLens :: NaiveLens User Int
ageLens = NaiveLens age
                    (\a s -> s { age = a })
                    (\f s -> s { age = f (age s) })
````


问题是现在我们需要为每个lens提供一个getter和一对setter了，即便我们可能只用到一个。

如果你已经使用Haskell有些日子了，你肯尼个已经见识过魔术函数const了。实际上也没啥魔术，它的类型是a -> b -> a，允许我们通过部分应用将 over :: (a -> a) -> s -> s 转化为 set :: a -> s -> s，set定义如下：

```` haskell
set :: NaiveLens s a -> a -> s -> s
set ln a s = over ln (const a) s
````

完整的代码如下：

```` haskell
data NaiveLens s a = NaiveLens
                        { view :: s -> a
                        , over :: (a -> a) -> s -> s }

set :: NaiveLens s a -> a -> s -> s
set ln a s = over ln (const a) s
````
### 有副作用的Lenses及更多

现在我们知道over确实有用，但是假如我们的修改方法需要产生一些副作用呢？例如我们可能想用从网络获取的新值覆盖当前的值。 我们可以像以前一样，添加一个名为overIO的函数看起来如下：

```` haskell
overIO :: (a -> IO a) -> s -> IO s
````

但是这意味着我们一对简单的getter和setter就又变为一个getter和两个setter了。暂不说,我们可能想要使用在更多类型上，而不仅仅是IO。现在它的类型看起来：

```` haskell
data NaiveLens s a = NaiveLens
        { view   :: s -> a
        , over   :: (a -> a) -> s -> s
        , overIO :: (a -> IO a) -> s -> IO s }
````

因此，我们可以将overIO写成更通用的方式，用Functor包装一下IO，得到以下类型：

```` haskell
overF :: Functor f => (a -> f a) -> s -> f s
````

鉴于篇幅有限，我要告诉你 overF 足以实现 view, set, over 和 overIO了。就是说我们不再需要Lens记录类型了，因此我们有了以下定义：

```` haskell
type Lens s a = Functor f => (a -> f a) -> s -> f s
````

需要注意的是，我们需要  RankNTypes 扩展的支持。

### 实现 over set view

让我们在继续之前总结一下先。我们以lens就是某个数据类型setter和getter的理念出发，之后我们用over函数泛化了setter。最好我们意识到在处理副作用问题上over还不够好，于是我们一定了overIO 并最终归纳为 van Laarhoven lens ： Functor f => (a -> f a) -> s -> f s。

直到现在，我只告诉你我们的新Lens是一个可以执行类似over, set和view这样操作的类型，但是我们只有需要证明这一点才能真正理解它。因此，我们将用到base库里的两个Fuctor的实例：Data.Functor.Identity 和 Control.Applicative.Const。让我们从最简单的一个开始，这个实现使用Identity函子。

#### Identity上的over 

首先来看一下Identity的实现：

```` haskell
newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
```

它的用处在于将一个值转化为函子，之后可以获取这个值。

over的最终类型是 over :: Lens s a -> (a -> a) -> s -> s 。我们可以读作：给定一个lens聚焦于 s 内的a 上，一个函数从a得到a， 一个s， 我们可以返回一个修改过的焦点。

```` hasekll
over :: Lens s a -> (a -> a) -> s -> s
over ln f s = _
````

