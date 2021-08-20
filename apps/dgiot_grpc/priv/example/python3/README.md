# emqx-exproto-python-sdk

The Python SDK for emqx-exproto

## Installation

There are the following ways to install this library

#### Install by Pypi

```bash
pip3 install emqx-exproto-sdk
```

#### Install from source code

```bash
git clone https://github.com/emqx/emqx-exproto-python-sdk.git

cd emqx-exproto-python-sdk

python3 setup.py install
```

## Get Started

- First of all, follow the step Installation to install dependencies
- Create your Python project

## Deploy

After compiled all source codes, you should deploy the sdk and your source files into emqx:

1. Ensure the emqx hosted machine has installed the `emqx-exproto-python-sdk`.
2. Copy your source code files. E.g: copy `example/demo.py` to `emqx/data/extension` directory.
3. Modify the `emqx/etc/plugins/emqx_exproto.conf` file. e.g:

    ```properties
    exproto.listener.protoname = tcp://0.0.0.0:7993
    exproto.listener.protoname.driver = python
    exproto.listener.protoname.driver_search_path = data/extension
    exproto.listener.protoname.driver_callback_module = demo
    ```
4. Execute `bin/emqx console` to start emqx and load the `emqx_exproto` plugin.

Use `telnet 127.0.0.1 7993` to establish a TCP connection and observe the console output.

## Interface

### Callbacks

**Connection Layer callbacks** (The Connection object represents a TCP/UDP Socket entity):

```python
// This function will be scheduled after a TCP connection established to EMQ X
//  or receive a new UDP socket.
on_connect(connection: Connection, connection_info: ConnectionInfo)

// This callback will be scheduled when a connection received bytes from TCP/UDP socket.
on_received(connection: Connection, data: bytes, state: any)

// This function will be scheduled after a connection terminated.
//
// It indicates that the EMQ X process that maintains the TCP/UDP socket
// has been closed. E.g: a TCP connection is closed, or a UDP socket has
// exceeded maintenance hours.
on_terminated(connection: Connection, reason: str, state: any)
```

**Pub/Sub Layer callbacks:**

```python
// This function will be scheduled when a connection received a Message from EMQ X
//
// When a connection is subscribed to a topic and a message arrives on that topic,
// EMQ X will deliver the message to that connection. At that time, this function
// is triggered.
on_deliver(connection: Connection, message_list: list)
```

### APIs

Similarly, AbstractExprotoHandler also provides a set of APIs to facilitate the use of the `emqx-exproto` APIs.

**Connection Layer APIs:**

```python
// Send a stream of bytes to the connection. These bytes are delivered directly
// to the associated TCP/UDP socket.
send(connection: Connection, data: bytes)

// Terminate the connection process and TCP/UDP socket.
terminate(connection: Connection)
```

**Pub/Sub Layer APIs:**

```python
// Register the connection as a Client of EMQ X. This `clientInfo` contains the
// necessary field information to be an EMQ X client.
//
// This method should normally be invoked after confirming that a connection is
// allowed to access the EMQ X system. For example: after the connection packet
// has been parsed and authenticated successfully.
register(connection: Connection, client_info: ClientInfo)

// The connection Publish a Message to EMQ X
publish(connection: Connection, message: Message)

// The connection Subscribe a Topic to EMQ X
subscribe(connection: Connection, topic: str, qos: int)
```

## Example

```python
from emqx.exproto.core import *
from emqx.exproto.abstract_handler import AbstractExProtoHandler
from emqx.exproto.connection import Connection, ConnectionInfo

import emqx.exproto.driver as driver

class SdkDemo(AbstractExProtoHandler):
    # In this class, you need to implement abstract AbstractExProtoHandler

    def on_connect(self, connection: Connection, connection_info: ConnectionInfo):
        print(connection)
        print(connection_info)

    def on_received(self, connection: Connection, data: bytes, state: any):
        print(connection)
        print(data)

    def on_terminated(self, connection: Connection, reason: str, state: any):
        print(connection)
        print(reason)

    def on_deliver(self, connection: Connection, message_list: list):
        print(connection)
        for message in message_list:
            print(message)

# set exproto_driver
driver.exproto_driver = SdkDemo()
```

## License

Apache License v2

## Author

- [Adek06](https://github.com/Adek06)
