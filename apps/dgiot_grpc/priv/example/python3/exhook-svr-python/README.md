# exhook-svr-python

This is a demo server written in python for exhook

## Prerequisites

- [Python](https://www.python.org) 3.5 or higher
- pip version 9.0.1 or higher

## Run

Install gRPC and gRPC Tools:

```
python -m pip install grpcio
python -m pip install grpcio-tools
```
Try to compile the `*.proto` files:

```
python -m grpc_tools.protoc -I./protos --python_out=. --grpc_python_out=. ./protos/exhook.proto
```

Run server:

```
python exhook_server.py
```
