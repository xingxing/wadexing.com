----
title: Why Go Is Not Good
----

[原文](http://yager.io/programming/go.html)

我喜欢Go, 我用Go做不少事儿（包括此刻我正在写的这个博客）。Go很好用。就像上面说的，Go不是一种优质的语言。它不次，只是不够好。

我们选用非优质语言必须得小心，因为如果莽撞行事我们没准接下来20年都要身陷其中。

我对比了Rust和Haskell(我认为的优质语言)。这里展示的所有问题都已经解决。

## 泛型编程
### 问题
我们所写的代码希望被应用到很多不同地方。如果我写了一个数字列表求和函数，如果它可以用在float列表，int列表，已经任何可以被就和的列表上就好了。要是它能够维持类型安全，并可以快速搞定int, float...列表求和的独立函数,那是好上加好。

### 一个好点子：基于泛型和参数化多态的约束

我认为Rust和Haskell使用的是目前最好的泛型编程系统。这通常关系到一个称为约束类型的系统。在Haskell中，这个系统被称为"type class",而在Rust里被称为"traits"。它是这样工作的：

(Rust, version 0.11)
```rust

n id<T>(item: T) -> T { 
   item 
}

```

(Haskell)
```haskell
id :: t -> t 
id a = a
```

在这个例子中，我们定义了一个泛型函数id,得到什么就返回什么。牛逼的是id对于什么都好使，而不单单只是某个类型。在Rust和Haskell中，id保持实参的类型信息，维护静态类型的安全性，并且没有因为泛型化增加额外的运行时开销。可以想象像这样实现一个clone函数。

我们也可以这样实现泛型数据结构。如:

(Rust)
```rust

struct Stack<T>{ 
   items: Vec<T> 
}

```

(Haskell)
```haskell

data Stack t = Stack [t]

```

再次重申，我们从泛型编程获得了静态类型安全和并且没有额外的运行时开销。

Now, if we want to write a generic function that actually does something to its arguments, we may need to somehow tell the compiler "only allow this function to work on arguments that allows this to be done to them". For example, if we wanted to make a function that added the three arguments and returned the sum, we need to tell the compiler that any arguments to the function must support addition. That looks like this:

```rust
fn add3<T:Num>(a:T, b:T, c:T)->T{ 
   a + b + c 
   }
```

```haskell

add3 :: Num t => t -> t -> t -> t 
add3 a b c = a + b + c

```

What we're telling the compiler is "The arguments to add3 can be of any type t, with the constraint that t must be a Num (Numeric type)." Because the compiler knows that Nums can be added, this code passes type checks. These constraints can also be used in data structure definitions. This is an elegant and easy way of doing 100% type-safe and flexible generic programming.

### Go的解决方案: interface{}

Go平庸的类型系统来的结果是，Go对泛型编程的支持十分糟糕。

想写一个通用函数足够简单。比方说你想写一个打印**可哈希对象**哈希的函数。

(Go)
```go

type Hashable interface { 
  Hash() []byte
}
  
func printHash(item Hashable) {
  fmt.Println(item.Hash())
}

```

现在printHash可以支持任意Haskable对象,并且这是静态类型安全的。不错。

可要是你想写一个通用数据结构呢？

(Go)

```go

type LinkedList struct {
   value interface{}
   next *LinkedList
}
      
func (oldNode *LinkedList) prepend(value interface{}) *LinkedList {
    return &LinkedList{value, oldNode}
}
         
func tail(value interface{}) *LinkedList {
    return &LinkedList{value, nil}
}
            
func traverse(ll *LinkedList) {
    if ll == nil {
        return
    }
    fmt.Println(ll.value)
    traverse(ll.next)
}
                             
func main() {
    node := tail(5).prepend(6).prepend(7)
    traverse(node)
}

```

注意到了吗？**value** 的类型是 **interface{}** 。**interface{}** 就是所谓 **”顶级类型“** ，也就是说其他类型皆为 **interface{}** の子类型。大略等价于Java里的Object。（＃‵′）靠。

在Go里所谓“正确”的泛型数据结构就是使用顶级类型然后把它们塞进数据结构里。2004年左右的Java就是这么干的。之后大家意识到这一个彻底失败的类型系统概念。当使用这样的数据结构的时候，你完全享受不了类型系统提供的任何福利。比如，下面这段无懈可击的代码:

```go
node := tail(5).prepend("Hello").prepend([]byte{1,2,3,4})
```

这段代码在结构化程序中无疑是不靠谱的。假如你期望一个整数链表，但同样，一些疲惫的背着deadline的程序员偶然把一个字符串放进来。由于通用数据结构在Go里不知道它们的值的类型，Go编译器不能纠正程序员，程序就这么挂了。

同样的问题在任何通用数据结构上都一样，不论是list, map, graph, tree, queue...

## 语言可扩展性
### 问题

高级语言通常拥有keywords和symbols来简化相对复杂的任务。比如，在很多语言中都有对于数据集合，像array，进行迭代的快捷方式。

(JAVA)

```java
for (String name : names) { ... }
```

(Python)

```python
for name in names: ...
```

如果我们可以在 ***任意*** 类型的集合上使用这样的语法该有多好啊,而不仅仅是语言提供的那些.

如果我们可以在我们的类型上定义如加这样的操作该多好,我们就可以这样:

(Python)

```python
point3 = point1 + point2
```

### 好点子:操作符即函数

A good solution is to make built-in operators correspond to functions/methods of a certain name, and to make keywords aliases to standard functions/methods.

在一些语言中,像Python,Rust和Haskell,允许重载操作符.你要做的就是为你的类定义一个函数,当使用特定操作符(如+)的时候,它就调用你定义的那个函数.在Python里,+操作符相当于 **__add__()** .在Rust里,+定义在Add trait(特性)里的 **add()**.在Haskell里,+相当于Num类型类里的 **(+)** 函数.

Many languages have a way to extend various keywords, like the for-each loop. Haskell doesn't have loops, but in languages like Rust, Java, and Python, there is usually some concept of an "Iterator" that allows for the usage of for-each loops on any kind of collection data structure.

A potential downside of this is that someone could define operators to do something completely unintuitive. For example, a crazy coder might define the "-" operation on two vectors to be the dot product. But that problem isn't specific to operator overloading. No matter the programming language, people can always write poorly-named functions.

### Go's solution: N/A

Go does not support operator overloading or keyword extensibility.

So what if we want to implement the range keyword for something else, like a tree or a linked list? Too bad. That is not part of the language. You can only range over builtins. Same thing with the make keyword. It can only be used to allocate space and initialize built-in data structures.

The closest thing to a flexible iterator keyword is building a wrapper around your data structure that returns a chan and then iterate over the chan, but that is slow, complicated, and bug-prone.

The justification for this is "It's very simple to understand, and the code that I read on the page is the code that is executed". That is, if Go allowed for extending things like range, it might cause confusion because the implementation details of this particular range mechanism might not be obvious. This argument makes little sense to me, because people are going to have to iterate over the data structure regardless of whether or not Go makes it easy and concise. Instead of hiding the implementation details inside a range() function, we have to hide the implementation details in some other helper function. Not a big improvement. All well-written code is easy to read, and most poorly-written code is hard to read. Obviously Go can't change that.

## Base Cases and Failure Conditions
### 问题

When working with recursive data structures, like linked lists or trees, we want a way to indicate that we've reached the end of the data structure.

When working with functions that may fail, or data structures that may have pieces of data missing, we want a way to indicate that we've reached some kind of failure condition.

### Go's Solution: Nil (and multiple return)

I'm going to discuss Go's solution first, because it gives context for the better solution discussed below.

Go has the null pointer (nil). I consider it a shame whenever a new language, tabula rasa, chooses to re-implement this unnecessary bug-inducing feature.

The null pointer has a rich and bug-riddled history. For historical and practical reasons, there is almost never useful data stored at the memory address 0x0, so pointers to 0x0 are generally used to represent some sort of special condition. For example, a function that returns a pointer might return 0x0 if it fails. Recursive data structures might use 0x0 to represent a base case (like the leaf of a tree, or the end of a linked list). This is what we use it for in Go.

However, using the null pointer in this way is unsafe. The null pointer is essentially a backdoor out of the type system; it allows you to create an instance of a type that isn't really of that type at all. It is an extremely common occurrence for a programmer to accidentally forget to account for the possibility that a pointer may be null, potentially leading to (at best) crashes and (at worst) exploitable vulnerabilities. The compiler can't easily protect against this, because the null pointer subverts the type system.

To Go's credit, it is idiomatically correct (and encouraged) to leverage Go's multiple return mechanism to return a second "failure" value if there is a possibility that a function will fail. However, this mechanism can easily be ignored or misused, and it generally isn't useful for representing base cases in recursive data structures.

### A Good Solution: Algebraic Types and Type-safe Failure Modes

Instead of violating the type system to represent error conditions and base cases, we can use the type system to safely encapsulate these cases.

Let's say we want to build a linked list type. We want to represent two possible cases: either we're not at the end of the linked list, and this linked list element has some data attached to it, or we are at the end of the linked list. A type-safe way to do this is to have separate types representing both of these cases, and then combine them into a single type (using algebraic data types). Let's say we have a type called Cons, representing a linked list element with some data, and End, representing the end of a linked list. We can write those like this:

(Rust)

```rust
enum List<T> { 
  Cons(T, Box<List<T>>),
  End
}
let my_list = Cons(1, box Cons(2, box Cons(3, box End)));
```

(Haskell)

```haskell
data List t = End | Cons t (List t)
let my_list = Cons 1 (Cons 2 (Cons 3 End))
```

Each type specifies a base case (End) for any recursive algorithms operating on the data structure. Neither Rust nor Haskell allow null pointers, so we know with 100% certainty that we will never experience a null pointer dereference bug (unless we do some really crazy low-level stuff).

These algebraic data types also allow for incredibly concise (and therefore evidently bug-free) code, through techniques like pattern matching (described later).

Now, what should we do if a function may or may not return something of a certain type, or a data structure may or may not contain a piece of data? That is, how can we encapsulate a failure condition in our type system? To solve this, Rust has something called Option, and Haskell has something called Maybe.

Imagine a function that searches an array of non-empty strings for a string starting with 'H', returns the first such string if it exists, and returns some kind of failure condition if it doesn't exist. In Go, we might return nil on failure. Here's how to do this safely, with no dangerous pointers, in Rust and Haskell.

(Rust)

```rust

fn search<'a>(strings: &'a[String]) -> Option<&'a str>{
  for string in strings.iter() {
      if string.as_slice()[0] == 'H' as u8 {
            return Some(string.as_slice());
      }
  }
  None
}

```

(Haskell)

```haskell
search [] = Nothing
search (x:xs) = if (head x) == 'H' then Just x else search xs
```


Instead of returning a string or a null pointer, we return an object that may or may not contain a string. We never return a null pointer, and programmers using search() know that it may or may not succeed (because its type says so), and they must prepare for both cases. Goodbye, null dereference bugs.

## Type Inference
### 问题

Sometimes it gets a little old specifying the type of every single value in our program. There are some cases where the value of a type should be obvious, such as

```ruby
int x = 5 
y = x*2
```

Clearly y should also be of type int. There are more complex situations where this is true as well, for example deducing the return type of a function based on the types of its arguments (or vice versa).

### A Good Solution: General Type Inference

Because Rust and Haskell are both based on the Hindley-Milner type system, they are both very good at type inference, and you can do cool things like this:

(Haskell)
```hakell
map :: (a -> b) -> [a] -> [b] 
let doubleNums nums = map (*2) nums 
doubleNums :: Num t => [t] -> [t]
```

Because the function (*2) takes a Num and returns a Num, Haskell can infer that the type of a and b is Num, and it can therefore infer that the function takes and returns a list of Num. This is much more powerful than the simple type inference supported by languages like Go and C++. It allows us to make a minimal number of explicit type declarations, and the compiler can correctly fill in the rest, even in very complex program structures.

### Go's solution: :=

Go supports the := assignment operator, which works like this:

(Go)
```go
foo := bar()
```

All this does is look at the return type of bar(), and set the type of foo to that. It's pretty much the same as this:

(C++)
```c++
auto foo = bar();
```

That's not really very interesting. All it does is shave off a few seconds of effort manually looking up the return type of bar(), and a few characters typing out the type in foo's declaration.

## Immutability
### 问题

Immutability is the practice of setting values once, at the moment of their creation, and never changing them. The advantages of this are pretty clear; if values are constant, every single bug caused by mutating a data structure in one place while using it in another is alleviated.

Immutability is also helpful for certain kinds of optimization.

### A Good Solution: Immutability By Default

Programmers should strive to use immutable data structures whenever possible. Immutability makes it much easier to reason about side effects and program safety, and strips away entire classes of bugs.

In Haskell, all values are immutable. If you want to modify a data structure, you have to create an entirely new data structure with the correct changes. This is still pretty fast because Haskell uses lazy evaluation and persistent data structures. Rust is a systems programming language, which means that it can't use lazy evaluation, which means that immutability isn't always as practical as it is in Haskell. Therefore, Rust makes variables immutable by default, but allows for mutability should it be needed. This is great, because it forces programmers to ask if they really need that variable to be mutable, which encourages good programming practice and allows for increased optimization by the compiler.

### Go's solution: N/A

Go does not support immutability declarations.

## Control Flow Structures
### 问题

Control flow structures are part of what separate programming languages from assembly. They let us use abstract patterns to direct a program's execution in a coherent way. Obviously all programming languages support some kind of control flow structures, or else we would not use them. However, there are a few nice control flow structures that I feel Go is missing.

### A Good Solution: Pattern Matching and Compound Expressions

Pattern matching is a really cool way of working with data structures and values. It's kind of like a case/switch expression on steroids. You can pattern match values like this:

(Rust)

```rust
match x {
  0 | 1 => action_1(), 
  2 .. 9 => action_2(), 
  _ => action_3() 
};
```

And you can deconstruct data structures like this:

(Rust)

```rust
deg_kelvin = match temperature { 
  Celsius(t) => t + 273.15, 
  Fahrenheit(t) => (t - 32)/1.8 + 273.15 
};
```

The previous example showed something sometimes called "compound expressions". In languages like C and Go, if statements and case/switch statements just direct the flow of the program; they don't evaluate to a value. In languages like Rust and Haskell, if statements and pattern matches actually can evaluate to a value, and this value can be assigned to things. In other words, things like if/else statements can actually "return" a value. Here's an example with an if statement:

(Haskell)
```haskell
x = if (y == "foo") then 1 else 2
```

### Go's Solution: C-style Valueless Statements

I don't want to short-change Go here; it does have some nice control flow primitives for certain things, like select for parallelism. However, it doesn't have compound expressions or pattern matching, which I'm a big fan of. Go only supports assignment from atomic expressions, like x := 5 or x := foo().

## Embedded Programming
### 问题

Writing programs for embedded systems is very different from writing programs for computers with full-featured operating systems. Some languages are much better suited to deal with the unique requirements of embedded programming.

I am confused by the seemingly non-trivial number of people advocating Go for programming robots. Go is not suitable for programming embedded systems, for a number of reasons. ***This section is not a criticism of Go. Go was not designed for embedded systems. This section is a criticism of people recommending Go for embedded programming.***


## Sub-Problem #1: The Heap and Dynamic Allocation

The heap is a region of memory that can be used to store an arbitrary number of objects created at runtime. We call usage of the heap "Dynamic Allocation".

It is generally unwise to use heap memory in embedded systems. The most practical reason for this is that the heap generally has a fair amount of memory overhead, and requires somewhat complex data structures to manage, neither of which are good when you're programming on an 8MHz microcontroller with 2KB of RAM.

It's also unwise to use the heap in real-time systems (systems that might fail if an operation takes too long), because the amount of time it takes to allocate and free memory on the heap is highly non-deterministic. For example, if your microcontroller is controlling a rocket engine, it would suck if you tried to allocate some space on the heap and it happened to take a few hundred milliseconds longer than usual, leading to incorrect valve timing and a massive explosion.

There are other aspects of dynamic allocation that do not lend themselves to effective embedded programming. For example, many languages that use the heap rely on garbage collection, which usually involves pausing the program for a bit and looking through the heap for things that aren't being used anymore, and then deleting them. This tends to be even less deterministic than simple heap allocation.


### A Good Solution: Making Dynamic Allocation Optional

Rust has a number of standard library features that rely on the heap, like boxes. However, Rust has compiler directives to completely disable any heap-using language features, and statically verify that none of these features are being used. It is entirely practical to write a Rust program with no heap usage.

### Go's solution: N/A

Go relies heavily on the usage of dynamic allocation. There is no practical way to constrain Go code to use only stack memory.  ***This is not a problem with Go. This is perfectly fine within Go's intended area of usage.***

Go is also not a real-time language. It is generally impossible to make strict guarantees about the execution time of any reasonably complex Go program. This may be a bit confusing, so let me be clear; Go is relatively fast, but it is not real-time. Those two are very different. Fast is nice for embedded programming, but the really important thing is that we need to be able to make guarantees about that maximum possible time something might take, which is not easily predicted when using Go. This is mostly due to Go's heavy usage of heap memory and garbage collection.

The same problems apply to languages like Haskell. Haskell is not suited for real-time or embedded programming because it has similarly heavy heap usage. However, I've never seen anyone advocating Haskell for robotics, so I've never needed to point this out.

## Sub-Problem #2: Unsafe Low-Level Code

When it comes to writing embedded programs, it is pretty much inevitable that you will eventually have to write some unsafe code (that does unsafe casts, or pointer arithmetic). In C or C++, doing unsafe things is really easy. Let's say I need to turn on an LED by writing 0xFF to the memory address 0x1234. I can just do this:

(C/C++)

```c
*(uint8_t*)0x1234 = 0xFF;
```

This is exceptionally dangerous, and only makes any sense whatsoever in very low-level systems programming. This is why neither Go nor Haskell have any easy way to do this; they are not systems languages.

### A Good Solution: Unsafe Code Isolation

Rust, with its focus on both safety and systems programming, has a good way of doing this: unsafe code blocks. Unsafe code blocks are a way of explicitly isolating dangerous code from safe code. Here's how we write 0xFF to the memory address 0x1234 in Rust:

(Rust)

```rust
unsafe{ 
  *(0x1234 as *mut u8) = 0xFF; 
}
```

If we tried to do that out of an unsafe code block, the compiler would complain loudly. This allows us to do the unfortunate but necessary dangerous operations inherent to embedded programming, while maintaining code safety as much as possible.

### Go's solution: N/A

Go is not intended to do this sort of thing, and thus has no built-in support for it.

## TL;DR

Now you may say "But why is Go not good? This is just a list of complaints; you can complain about any language!". This is true; no language is perfect. However, I hope my complaints reveal a little bit about how

- Go doesn't really do anything new. 
- Go isn't well-designed from the ground up. 
- Go is a regression from other modern programming languages. 
