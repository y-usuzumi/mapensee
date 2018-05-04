% 为小四准备的Python大纲
% Yukio Usuzumi
% 2018/05/04

> 重要声明：
> 本文全部内容基于CPython 3.6.5!!!!!!


## Python是什么

1. Python是个*动态类型*、*强类型*的*脚本语言*
1. Python是编译执行的（你问10个人有9个说解释执行，除了我）
1. Python是机器学习领域的大哥级语言
1. 动态语言一时爽，重构火葬场

```python
a = 3
a + "123"  # Error

class A:
    pass

a = A()
print(type(A))  # <class 'type'>
```


## Python不是什么

1. Python不是Python语言的解释器，CPython, PyPy等才是
1. 如果对方讲Python，但想表达的是某个具体实现，默认将其理解为CPython，同时
   请在心里骂他100遍傻逼


## CPython是什么

1. Python的官方、参考实现
1. 用得最多的实现


## CPython不是什么

1. 不是一个高性能的实现，PyPy才是


## Python语言的特点

1. 简单
1. 高级语言
1. 面向对象
1. 可扩展
1. 可嵌入
1. 内置库巨牛逼
1. 所有东西都tm是对象（`isinstance(anything, object)`为真）


## Python长啥样

1. 缩进党
1. 冒号
3. 各种双下划线包围标示的魔法变量/函数/方法


## 注释

用`#`。。没有多行注释。。。辣鸡。。。


## 常用数据类型

```python
1, 2, 3, ...                # int (支持超长整数，Python 2是傻逼)
0b1010, 0o12, 0xA, 0xa      # int (二进制、八进制、十六进制的10)
1., -3.14, 1e100            # float (没有double这种东西)
1+2j                        # complex (你应该用不到)
True / False                # bool
"Hello"                     # str
u"Hello"                    # str (Python2是傻逼)
b"Hello"                    # bytes (Python2是傻逼)
"Hello"[0]                  # "H"，还是str，没有char这种东西
(), (1,), (2, "Hello")      # tuple，(1,)不能写作(1)
set(), {1}, {2, "Hello"}    # set，set()不能写作{}
[], [1], [2, "Hello"]       # list
{}, {"a": 1, 123: "Hello"}  # dict
```

## n种字符串的表达方式

```python
'Yukio says: "I love you, \'Xiaosi\'."'
"Yukio says: 'I love you, \"Xiaosi\".'"
'''Yukio says: "I love you, 'Xiaosi.'"'''
"""Yukio says: "I love you, 'Xiaosi.'""""
```

## n处字符串的插值方式

```python
'''%s says: "I love you, '%s'."''' % ('Yukio', 'Xiaosi')    # 一定要是tuple
'''%(me)s says: "I love you, '%(her)s'."''' % {'me': 'Yukio', 'her': 'Xiaosi'}  # 一定要是dict
'''{} says: "I love you, '{}'."'''.format('Yukio', 'Xiaosi')
'''{me} says: "I love you, '{her}'."'''.format(me='Yukio', her='Xiaosi')
```

猜猜上面的插值例子中，怎么分别对`%`和`{`进行转义？


## 算术

| 运算符     | 运算         |
| :--------: | :----------: |
| +          | 加           |
| -          | 减           |
| *          | 乘           |
| /          | 除           |
| **         | 乘方         |
| //         | 整除         |
| %          | 模           |
| &          | 按位与       |
| &#124;     | 按位或       |
| ^          | 按位异或     |


## 怎么执行？

### 方法一

```
<python解释器> <python源文件>
```

### 方法二

a.py:
```python
#!<python解释器>

# Python代码
```
当a.py具有可执行权限后：
```
./a.py
```

### 其他方法

.pyc

## 执行什么？

1. 会按次序执行当前文件内的所有内容
2. 若当前文件为入口文件，则`__name__ == '__main__'`的判断会成立

```python
# 可粗略地理解为C语言的main函数。。。详情参见下文模块相关内容
if __name__ == '__main__':
    print("Hello world!")
```

## 条件

```python
if <条件1>:
    <代码>
elif <条件2>:
    <代码>
elif <条件3>:
    <代码>
else:
    <代码>
```

## 循环

```python
for <元素> in <迭代器容器>:
    <代码>
else:
    <未使用break时会执行的代码>
```

```python
while <条件>:
    <代码>
else:
    <未使用break时会执行的代码>
```

## 异常

```python
try:
    <代码>
except [<某异常类型1> [as <异常标识>]:
    <代码>
except [<某异常类型2> [as <异常标识>]:
    <代码>
finally:
    <代码>
```

```python
raise SomeException    # 抛出异常
raise SomeException()  # 同上
```
抛出异常可以抛出类型或实例。当抛出类型时会自动创建实例，具体得看文档。


## 函数

### 定义函数

```python
def func(a, b, *args, c, d, **kwargs):
    # a, b为位置参数
    # args为可变位置参数
    # c, d为关键字参数
    # kwargs为可变关键字参数
    print("A: %s" % a)
    print("B: %s" % b)
    print("Args: %s" % (args,))
    print("C: %s" % c)
    print("D: %s" % d)
    print("Kwargs: %s" % kwargs)
    
    # 函数可返回多个值，此时调用方接收到的是元组
    return 'a', 'b', 'c'
    
ret = func(1, 2, 3, 4, c=5, d=6, e=7, f=8)
print("Result: %s" % (ret,))
```
输出：

```
A: 1
B: 2
Args: (3, 4)
C: 5
D: 6
Kwargs: {'e': 7, 'f': 8}
Result: ('a', 'b', 'c')
```

### 匿名函数

不支持多行。。。相当鸡肋

```python
a = lambda x: x + 1
```


### 调用函数

```python
func(1, 2, 3, 4, c=5, d=6, e=7, f=8)

# a=1, b=2, args=(100, 3, 4), c=5, d=6, kwargs={'e': 7, 'f': 8}
func(1, 2 100, *[3, 4], **{'c': 5, 'd': 6, 'e': 7, 'f': 8})
```


## 类

```python
class Shape:
    def get_area(self):
        raise NotImplementedError
        
        
class Rectangle(Shape):
    def __init__(self, width, height):
        self.width = width
        self.height = height
        
    def get_area(self):
        return self.width * self.height
        
        
class Circle(Shape):
    pi = 3.1415926535897932384626

    def __init__(self, radius):
        self.radius = radius
        
    def get_area(self):
        return self.pi * self.radius * self.radius
        
        
class Square(Rectangle):
    def __init__(self, side_length):
        super().__init__(side_length, side_length)
        
rect = Rectangle(3, 4)
print(rect.get_area())  # 12

circle = Circle(10)
print(circle.get_area())  # 314.1592653589793

square = Square(5)
print(square.get_area())  # 25
```


## 静态方法和类方法

```python
import math


class Math:
    @staticmethod
    def sin(c):
        return math.sin(c)
        
    @classmethod
    def cos(cls, c):
        return math.cos(c)
        
        
Math.sin(math.pi / 2)  # OK
Math.cos(math.pi / 2)  # OK
m = Math()
m.sin(math.pi / 2)  # OK
m.cos(math.pi / 2)  # OK
```

直接在类上调用实例方法是不行的。


## 属性

```python

class Math:
    @property
    def pi(self):
        return 3.1415926535897932384626
        
m = Math()
m.pi  # 3.1415926535897932384626
```

上面实现了一个getter，有关setter和deleter，看文档。


## 构造函数

其实呢，不存在构造函数一说。。。
和初始化对象有关的函数有两个：`__new__`和`__init__`

`__new__`是静态方法，不管你加不加`staticmethod`装饰器。第一个参数一定会接收当前类型。
若`__new__`返回该类实例，则调用该实例的`__init__`方法，并按次序传入调用`__new__`时其余的参数。
否则，不再调用`__init__`方法。

通常你不需要用`__new__`。


## 多重继承和方法解析顺序 (MRO)

不需要了解。


## 推导式

又可以轻松一点了。

适用于Python的三大集合类型：list, set, dict

```python
# 列表推导式
[str(i) for i in range(10) if i % 2 == 0]           # ['0', '2', '4', '6', '8']
# 集合推导式
{i % 3 for i in range(10)}                          # {0, 1, 2}
# 字典推导式
{k: v for k, v in zip(['a', 'b', 'c'], [1, 2, 3])}  # {'a': 1, 'b': 2, 'c': 3}
# 生成器推导式：
(str(i) for i in range(10) if i % 2 == 0)           # <generator object <genexpr> at 0x7fe49cc3a830>
```


## 迭代器

*迭代器容器*是指可以用在`for`循环中的容器。
迭代器容器实现如下方法：

```python
container.__iter__()  # 返回迭代器
```

满足迭代器协议的对象为迭代器。
迭代器协议为如下两个方法：

```python
iterator.__iter__()  # 返回自己
iterator.__next__()  # 返回下一个元素或raise StopIteration表示迭代结束
```
之所以迭代器要实现`__iter__`是因为这可以让迭代器本身支持`for`循环。


## 生成器

生成器也是一种迭代器，所以它可以用在`for`循环中。
生成器可以通过包含`yield`关键字的函数创建：

```python
def fib():
    a, b = 1, 1
    while True:
        yield a
        a, b = b, a + b
        
fib_seq = fib()
print(next(fib_seq) for _ in range(10))
```

也可以使用生成器推导式（上面讲过了）。


## del

用来删除变量，或列表/字典中的元素。

```python
a = 3
del a
a                     # NameError

a = [1, 2]
del a[0]
a                     # [2]

a = {'a': 1, 'b': 2}
del a['b']
a                     # {'a': 1}
```

当变量定义了`__del__`方法时，可以自定义其行为。


## 装饰器

装饰器是一个函数，它接收被装饰的东西，返回新东西，并将其绑定到被装饰的名称上。

```python
def shitty_decorator(func):
    return 3
    
    
@shitty_decorator
def whoami():
    print("Who am I?")


print(whoami)    # 3
```

装饰器可以叠加多个，每一个都接收下方装饰器返回的新物件。
装饰器可以以函数调用的方式进行，相当于首先调用函数，然后将其返回的装饰器装饰在目标物件上，如：

```python
def shitty_decorator(value):
    def wrapper(func):
        return value

    return wrapper
        
        
@shitty_decorator(100)
def whoami():
    print("Who am I?")
    
    
print(whoami)    # 100
```


## 描述器

暂时不需要了解。


# 好累呀，先睡了。。。
