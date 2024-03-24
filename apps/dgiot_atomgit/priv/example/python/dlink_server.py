# Copyright 2015 gRPC authors.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
"""The Python implementation of the GRPC helloworld.Greeter server."""
import json
from concurrent import futures
import logging
import grpc
import dlink_pb2
import dlink_pb2_grpc
import base64
import datetime

class Dlink(dlink_pb2_grpc.DlinkServicer):
    def Payload(self, request, context):
        print(datetime.datetime.now().strftime('%Y-%m-%d-%H-%M-%S'))
        print( request.cmd)
        print( request.data)
        return dlink_pb2.PayloadResponse(topic="dgiot/ddd", payload="ddddd")


def serve():
    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    dlink_pb2_grpc.add_DlinkServicer_to_server(Dlink(), server)
    server.add_insecure_port('[::]:30051')
    print( "start")
    server.start()
    print( "start dd")
    server.wait_for_termination()


if __name__ == '__main__':
    logging.basicConfig()
    serve()


