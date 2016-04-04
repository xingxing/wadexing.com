------
title: Part III GIL能让你的Ruby代码线程安全吗？
------

围绕着MRI的GIL，ruby社区中有一些错误观念。要是你今天只想从这篇文章获取一个观点，那就是：GIL不会使你的Ruby代码线程安全。

但请别这么相信我。

这个系列一开始只是为了从技术层面上了解GIL。Part I解释了竞争条件是如何在实现MRI的C源码中发生的。还有，GIL貌似排除了风险，至少我们看到 **Array#<<**方法是这样。

Part II证实了GIL的作为，实际上，它使得MRI的原生C方法实现原子化了。换而言之，这些原生方法是对竞争条件免疫的。这个保证只针对MRI的C原生方法，你自己写的那些Ruby可不行。
于是我得到一个遗留问题：

< GIL能否保证我们的Ruby代码是线程安全的？

我已经回答过这个问题了。现在我想确保谣言止于智者。

### 归来的竞争条件

竞争条件发生在一些数据块在多个线程之间共享，并且这些线程企图同时在数据上进行操作的时候。当发生时没有一种同步机制，比如锁，你的程序会开始做一些意料之外的事，并且数据也会遗失。

让我们回过头来回顾一下这种竞争状态是如何发生的。我们将使用如下Ruby代码作为本节的示例：

```ruby
class Sheep
  def initialize
    @shorn = false
  end

  def shorn?
    @shorn
  end

  def shear!
    puts "shearing..."
    @shorn = true
  end
end
```
这个类定义应该很常见。一头**羊**在初始化的时候是没被薅过的。**shear!**方法执行薅羊毛并标记这头羊为薅过。

![](https://ruby-china-files.b0.upaiyun.com/photo/2015/fbdc08265b90e42609c113f455832f14.jpg)

```ruby
sheep = Sheep.new

5.times.map do
  Thread.new do
    unless sheep.shorn?
      sheep.shear!
    end
  end
end.each(&:join)
```
这一小段代码创建了头羊并且衍生出5个线程。每个线程竞相检查羊是不是被薅过？要是没有，就调用 **shear!** 方法。

以下结果是我在MRI2.0里多次执行得到的。

```shell
$ ruby check_then_set.rb
shearing...
$ ruby check_then_set.rb
shearing...
shearing...
$ ruby check_then_set.rb
shearing...
shearing...
```

有的时候一只羊被薅了两回。
