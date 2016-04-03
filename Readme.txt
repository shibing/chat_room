#chat-room: 基于erlang的聊天室程序

学习erlang练手写的聊天室小程序，支持以下功能：
 - 匿名登录聊天室
 - 修改群聊昵称
 - 创建新的聊天室
 - 加入其他聊天室
 - 给处于同一聊天室的人群发消息

## 使用方法

首先下载源代码：
```
git@github.com:shibing/chat_room.git
```
然后用下面的命令启动启动一个erlang shell：
```
erl -pa ./ebin -config ./sys
```
启动chat_room应用：
```
application:start().
```

