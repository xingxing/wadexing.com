----
title: View Patterns
----

[原文](https://ocharles.org.uk/blog/posts/2014-12-02-view-patterns.html)

View patterns 扩展了模式匹配的能力，使得函数的返回值上也可以做模式匹配。来看一个简单的例子，用Map来统计Hackage上包的下载量。一上来，我们将用lens库赖获取下载数量。通常，我们会这样写：

```` haskell

lensDownloadsOld :: Map HaskellPackage Int -> Int
lensDownloadsOld packages =
        case M.lookup "lens" packages of
            Just n -> n
            Nothing -> 0

````

注意调用这个函数做的第一件事就是模式匹配。可知，这个晦涩的lensDownloads函数定义由两个方程定义：1、当一个包已经有下载量 2、这个包还没被下载时 (例如：统计新的一批包)。
用view patterns，我们可以将这个检查从右手移到左手边：

```` haskell

lensDownloads :: Map HaskellPackage Int -> Int
lensDownloads (M.lookup "lens" -> Just n) = n
lensDownloads _                           = 0

````

现在我们的检查方法通过两个方程式来定义，和我们所期待的一样。View patterns 允许我们把下载统计当做一个不同的数据类型来"看"，这样的话，我们通过在key lens在值上成像(lens包的术语)，就把这个map当成Maybe Int类型一样"看"。


诚如所见，一个观测模式由两个部分定义而成。像自身，这是一个部分应用函数；和一个在此函数结果上应用的模式匹配。这样一来，我们给定一个 Map HaskellPackage Int 得到视图 M.lookup "lens" :: Map HaskellPackage Int -> Maybe Int。

我们在这个Maybe Int上做模式匹配偶，Just的情况下绑定下载数量到变量n。同样值得注意的是，如果上面的模式匹配失败，会跳到下lensDownloads的一个模式。GHC会小心翼翼地穷举所有模式，所以我们就专心于所有可能条件即可。


最终，可能有点无趣，必须为每个包都写一个这样的函数。所以也许我们应该将包名抽象出来。用view patterns，我们的视图函数可以在视图模式的左边绑定变量。因此，泛化的download-lookup函数就像这样:

```` haskell

downloadsFor :: HaskellPackage -> Map HaskellPackage Int -> Int
downloadsFor pkg (M.lookup pkg -> Just downloads) = downloads
downloadsFor _   _                                = 0

````
