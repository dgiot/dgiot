# [Protobuf 之 proto 基础语法](https://www.cnblogs.com/sherlock-lin/p/16522652.html)

目录

* [1、说明](https://www.cnblogs.com/sherlock-lin/p/16522652.html#1%E8%AF%B4%E6%98%8E)
* [2、字段类型](https://www.cnblogs.com/sherlock-lin/p/16522652.html#2%E5%AD%97%E6%AE%B5%E7%B1%BB%E5%9E%8B)
* [3、字段规则](https://www.cnblogs.com/sherlock-lin/p/16522652.html#3%E5%AD%97%E6%AE%B5%E8%A7%84%E5%88%99)
* [4、字段编号](https://www.cnblogs.com/sherlock-lin/p/16522652.html#4%E5%AD%97%E6%AE%B5%E7%BC%96%E5%8F%B7)
* [5、注释](https://www.cnblogs.com/sherlock-lin/p/16522652.html#5%E6%B3%A8%E9%87%8A)
* [6、类型](https://www.cnblogs.com/sherlock-lin/p/16522652.html#6%E7%B1%BB%E5%9E%8B)
  * [6.1、message](https://www.cnblogs.com/sherlock-lin/p/16522652.html#61message)
  * [6.2、service](https://www.cnblogs.com/sherlock-lin/p/16522652.html#62service)
* [7、枚举 enum](https://www.cnblogs.com/sherlock-lin/p/16522652.html#7%E6%9E%9A%E4%B8%BEenum)
* [8、保留字段](https://www.cnblogs.com/sherlock-lin/p/16522652.html#8%E4%BF%9D%E7%95%99%E5%AD%97%E6%AE%B5)
* [9、import](https://www.cnblogs.com/sherlock-lin/p/16522652.html#9import)
  * [9.1、protoc 指令](https://www.cnblogs.com/sherlock-lin/p/16522652.html#91protoc%E6%8C%87%E4%BB%A4)

# 1、说明

示例中的 proto 文件描述了一个数据结构，遵循 Protobuf 语法

示例：

```protobuf
message TestOne {
	required string name = 1;
	optional int32 age = 2;
}
```

# 2、字段类型

| type     | C++ type | Java Type  | Python Type | description                                                                                                                                            |
| -------- | -------- | ---------- | ----------- | ------------------------------------------------------------------------------------------------------------------------------------------------------ |
| double   | double   | double     | float       |                                                                                                                                                        |
| float    | float    | float      | float       |                                                                                                                                                        |
| int32    | int      | int        | int         |                                                                                                                                                        |
| uint32   | uint32   | int        | int/long    |                                                                                                                                                        |
| int64    | int64    | long       | int/long    |                                                                                                                                                        |
| uint64   | uint64   | long       | int/long    |                                                                                                                                                        |
| sint32   | int32    | int        | int         | 存数据时引入 zigzag 编码``（Zigzag(n) = (n << 1) ^ (n >> 31)``解决负数太占空间的问题``**正负数最多占用 5 个字节，内存高效** |
| sint64   | int64    | long       | int/long    |                                                                                                                                                        |
| fixed32  | uint32   | int        | int/long    | 4 byte``抛弃了可变长存储策略``适用与存储数值较大数据                                                                                     |
| fixed64  | uint64   | long       | int/long    |                                                                                                                                                        |
| sfixed32 | int32    | int        | int         |                                                                                                                                                        |
| sfixed64 | int64    | long       | int/long    |                                                                                                                                                        |
| bool     | bool     | boolean    | bool        |                                                                                                                                                        |
| string   | string   | String     | unicode     |                                                                                                                                                        |
| bytes    | string   | ByteString | bytes       |                                                                                                                                                        |

1. 负数使用 **sint** ；
2. 数值较大，使用 **fixed** ；

# 3、字段规则

示例中的 required 和 optional 都是字段规则，规则有以下几个：

| 字段规则 | 说明                                                                                  |
| -------- | ------------------------------------------------------------------------------------- |
| required | 格式良好的 message 必须包含该字段一次                                                 |
| optional | 格式良好的 message 可以包含该字段零次或一次（不超过一次）                             |
| repeated | 该字段可以在格式良好的消息中重复任意多次（包括零）``其中重复值的顺序会被保留。 |

**optional** 字段是可选字段，如果不设定数据，则需要有一个默认值以避免出错，指定方式：

```protobuf
message TestOne {
	required string name = 1;
	optional int32 age = 2 [default = 100];
}
```

# 4、字段编号

如示例中，每个字段都有一个编号，这些编号是  **唯一的** 。

* 编号 1-15 会占用一个字节，16-2047 占用两个字节，因此尽可能使用 1-15；
* 字段编号在 message 被使用后，就不能再更改；

# 5、注释

支持使用 C/C++ 风格的注释

# 6、类型

## 6.1、message

* **message** 表示一个类型，后面的名称将会是类名；
* 一个 proto 文件可以定义多种 message 类型，但尽可能减少一个 proto 多类型的情况；

> 组合 messages 会导致膨胀虽然可以在单个 .proto 文件中定义多种 messages 类型（例如 message，enum 和 service），但是当在单个文件中定义了大量具有不同依赖关系的 messages 时，它也会导致依赖性膨胀。建议每个 .proto 文件包含尽可能少的 message 类型。

## 6.2、service

一般在和 RPC 系统一起使用，示例：

```protobuf
service TestService {
	rpc Test (TestRequest) returns (TestResponse);
}
```

# 7、枚举 enum

枚举定在 **message** 内部

示例：

```protobuf
message TestOne {
	required string name = 1;
	optional int32 age = 2 [default = 100];

	enum Country {
		China = 0;
		USA = 1;
	}
	optional Country country = 3 [default = China]
}
```

# 8、保留字段

示例：

下面例子，字段编号 2/15/9/11 曾经使用过，保留；字段名 foo/bar 曾经使用过，保留

```protobuf
message Foo {
    reserved 2, 15, 9 to 11;
	reserved "foo", "bar";
}
```

如果一个字段不再需要，如果删除或者注释掉，则其他人在修改时会再次使用这些字段编号，那么旧的引用程序就可能出现一些错误，所以使用保留字段，保留已弃用的字段编号或字段名 ，可解决该问题

# 9、import

如果一个 proto 文件需要使用另一个 proto 文件中的定义(message/service)，就需要使用 **import** 引入其他的 proto 文件

**import** 使用 **public** 属性，控制依赖是否可以传递，被 **public** 修饰的可以传递

示例：

```protobuf
//second.proto
import "message/aaa.proto";
import public "message/bbb.proto";
```

```protobuf
//test_one.proto
import "message/second.proto";

message TestOne {
	required string name = 1;
	optional int32 age = 2 [default = 100];
}
```

如上示例中，test_one.proto 中可以使用 bbb.proto 的类型，但不能使用 aaa.proto 中的类型

## 9.1、protoc 指令

protoc 的参数 **-I** 和 **--proto_path** 用于传递 **import** 的路径

当不同的 proto 文件位于不同的路径时，import 可以不传递相对路径，而使用 -I 或者 --proto_path 传递 import 的查找路径
