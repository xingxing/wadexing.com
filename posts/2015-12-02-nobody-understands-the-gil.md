----
title: 无人知晓的GIL
----

### 原文:
[http://www.jstorimer.com/blogs/workingwithcode/8085491-nobody-understands-the-gil](http://www.jstorimer.com/blogs/workingwithcode/8085491-nobody-understands-the-gil)

我的大半生都是在Ruby社区中度过的，然而MRI中臭名昭著的GIL对我而言却一直是个叵测的家伙。这是一个关于线程安全的故事，最终我们会真相大白，

最初听人提及GIL时，我不知道它是如何工作的，它做了什么事，甚至也不知道它为什么会存在。我只知道这是个蠢主意，因为它限制了并行，换句话说就是"曾经辉煌"，因为它让我的代码线程安全。后来，我总算学会了如何去爱多线程，也意识到了现实远比我设想的复杂。

我要知其然，更知其知其所以然，GIL到底是怎么工作的？但是，GIL没有规程(specification)[1]可循，亦没有文档可看。本质上说它就是一个未知行为；一个MRI的实现细节。Ruby核心组没有对它将如何工作予以承诺或担保。

也许我有点儿超前了。

如果你对GIL一无所知，花30秒钟读读下面这个简介吧：

> MRI里有个东西叫全局解释器锁(global interpreter lock)。这个锁环绕着Ruby代码的执行。即是说在一个多线程的上下文中，在任何时候只有一个线程可以执行Ruby代码。

> 因此，假如一台8核机器上跑着8个线程，在特定的时间点上也只有一个线程和一个核心在忙碌。GIL一直保护着Ruby内核，以免竞争条件造成数据混乱。把警告和优化放一边，这就是它的主旨了。


### 问题

回到2008， Ilya Grigorik 的 [《Ruby里的并行神话》](http://www.igvita.com/2008/11/13/concurrency-is-a-myth-in-ruby/)给了我对GIL的高层次理解。即使我学确实学到了更多的Ruby多线程技术，但是这个高层次认识只是对我的单方灌输。真见鬼，我最近还写了一本关于[Ruby里多线的书](http://www.jstorimer.com/products/working-with-ruby-threads)呢,但是对于GIL我就理解了这么点儿？

问题是用些"微言大义"的认识，我没法回答有深度的技术问题。特别是，我想知道GIL是否提供了关于线程安全的任何保障。让我来示范一下。

### 数组附加是非线程安全的

几乎没什么事在Ruby里是隐式线程安全的。以附加数组为例：

```ruby
array = []

5.times.map do
  Thread.new do
    1000.times do
      array << nil
    end
  end
end.each(&:join)

puts array.size
```

这里有5个线程共享一个数组对象。每个线程将nil放入数组1000次。因此，数组里应该有5000个元素，对吧？

```shell
$ ruby pushing_nil.rb
5000

$ jruby pushing_nil.rb
4446

$ rbx pushing_nil.rb
3088
```

:(

即使这个微不足道的例子，也足以揭示Ruby里的一个操作并非隐式线程安全的。或许是？实际上发生什么了呢？

请注意MRI的结果是正确的, 5000。但是JRuby和Rubinius都错了。如果你再跑一遍，你很可能会看到MRI依然正确，但是JRuby和Rubinius给出了不同的错误结果。

这些不同的结果是GIL造成的。因为MRI有GIL，即使同时有5个线程在跑，在一个时间点上也只有一个线程是活动的。JRuby和Rubinius没有GIL，所以当你有5个线程在跑，你就真的有5个线程通过获取核心在并行地跑。


在并行的Ruby实现中，这5个线程逐句通过代码，而这是非线程安全的。它们最终互相干扰，最终腐化底层数据。

### 多线程如何腐化数据

这怎么可能？我还以为Ruby会罩着我们呢，对吧？相对于通过高层次的解释来阐述技术细节，我更倾向于向你展示这在技术上的可能性。

无论你是用MRI,JRuby或是Rubinius，Ruby语言是用其他语言实现的。 MRI是用C实现的，JRuby用Java,Rubinius是Ruby和C++的混合体。于是当你有这样一个Ruby操作时：

```ruby
array <<< nil
```

实际上在底层实现上会扩展为一大堆代码。例如，下面是Array#<<在MRI中的实现：

```C
VALUE

rb_ary_push(VALUE ary, VALUE iterm)
{
   long idx = RARRAY_LEN(ary);

   ary_ensure_room_for_push(ary, 1);
   RARRAY_ASET(ary, idx, item);
   ARY_SET_LEN(ary, idx + 1);
   return ary;
}

```

注意至少4个不同的底层操作。

1. 获取数组的当前长度
2. 检查数组里是否有空间容纳其他元素。
3. 将元素附件到数组
4. 将数组的长度属性置为原值+1。

每个操作还回调用别的函数或者宏。我提到这些是为了向你们展示多线程是如何能够破坏数据的。在但线程环境中，你可以观察并简单地跟踪这个短代码的轨迹。

话句话说，我们已经习惯了以线性的方式逐句执行代码并推断"真实世界"的状态。我们通常就是这么写代码的。

当多线程乱入，这就不可行了。这很像物理变化的规则。当有两个线程，每个线程维护这个自己的代码轨迹。由于线程共享同一个内存空间，而这些线程可以同时改变"真实世界"中的状态。

一个线程可能会打扰另一个线程，从此改变事物的状态，之后原先的线程完全不知状态已经被改变了。

这里是我的小系统的基本状态：

![](http://cdn.shopify.com/s/files/1/0110/8792/files/append_base_grande.png?653)

有两个活跃线程，同时进入这个函数(C语言中的)。将1-4步看做MRI中Array#<<的伪代码实现，之前你见过的。一旦两个线程进入这个函数，就可能出现一系列事件，假设从线程A开始：


![](http://cdn.shopify.com/s/files/1/0110/8792/files/append_arrows_grande.png?655)

这看着更复杂了，但是只要跟着箭头的方向，你就可以穿过这个流程。我还加了在每个步骤上一些标签从每个线程的角度来显示各种状态。

这是一种可能性。

于是线程A沿着函数的常规路径执行，但当执行到步骤3时，发生了上下文切换！线程A被暂停在当前位置。当线程B接管了



[1] 参考 《编程本源》裘宗燕 译


