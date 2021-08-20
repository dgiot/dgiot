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
"""The Python implementation of the GRPC exhook server."""

from concurrent import futures
import logging
from multiprocessing.sharedctypes import Value

import grpc

import exhook_pb2
import exhook_pb2_grpc

class HookProvider(exhook_pb2_grpc.HookProviderServicer):

    def OnProviderLoaded(self, request, context):
        specs = [exhook_pb2.HookSpec(name="client.connect"),
                 exhook_pb2.HookSpec(name="client.connack"),
                 exhook_pb2.HookSpec(name="client.connected"),
                 exhook_pb2.HookSpec(name="client.disconnected"),
                 exhook_pb2.HookSpec(name="client.authenticate"),
                 exhook_pb2.HookSpec(name="client.check_acl"),
                 exhook_pb2.HookSpec(name="client.subscribe"),
                 exhook_pb2.HookSpec(name="client.unsubscribe"),

                 exhook_pb2.HookSpec(name="session.created"),
                 exhook_pb2.HookSpec(name="session.subscribed"),
                 exhook_pb2.HookSpec(name="session.unsubscribed"),
                 exhook_pb2.HookSpec(name="session.resumed"),
                 exhook_pb2.HookSpec(name="session.discarded"),
                 exhook_pb2.HookSpec(name="session.takeovered"),
                 exhook_pb2.HookSpec(name="session.terminated"),

                 exhook_pb2.HookSpec(name="message.publish"),
                 exhook_pb2.HookSpec(name="message.delivered"),
                 exhook_pb2.HookSpec(name="message.acked"),
                 exhook_pb2.HookSpec(name="message.dropped")
                ]
        return exhook_pb2.LoadedResponse(hooks=specs)

    def OnProviderUnloaded(self, request, context):
        return exhook_pb2.EmptySuccess()

    def OnClientConnect(self, request, context):
        return exhook_pb2.EmptySuccess()

    def OnClientConnack(self, request, context):
        return exhook_pb2.EmptySuccess()

    def OnClientConnected(self, request, context):
        return exhook_pb2.EmptySuccess()

    def OnClientDisconnected(self, request, context):
        return exhook_pb2.EmptySuccess()

    def OnClientAuthenticate(self, request, context):
        reply = exhook_pb2.ValuedResponse(type="STOP_AND_RETURN", bool_result=True)
        return reply

    def OnClientCheckAcl(self, request, context):
        reply = exhook_pb2.ValuedResponse(type="STOP_AND_RETURN", bool_result=True)
        return reply

    def OnClientSubscribe(self, request, context):
        return exhook_pb2.EmptySuccess()

    def OnClientUnsubscribe(self, request, context):
        return exhook_pb2.EmptySuccess()

    def OnSessionCreated(self, request, context):
        return exhook_pb2.EmptySuccess()

    def OnSessionSubscribed(self, request, context):
        return exhook_pb2.EmptySuccess()

    def OnSessionUnsubscribed(self, request, context):
        return exhook_pb2.EmptySuccess()

    def OnSessionResumed(self, request, context):
        return exhook_pb2.EmptySuccess()

    def OnSessionDiscarded(self, request, context):
        return exhook_pb2.EmptySuccess()

    def OnSessionTakeovered(self, request, context):
        return exhook_pb2.EmptySuccess()

    def OnSessionTerminated(self, request, context):
        return exhook_pb2.EmptySuccess()

    def OnMessagePublish(self, request, context):
        nmsg = request.message
        nmsg.payload = b"hardcode payload by exhook-svr-python111 :)"

        reply = exhook_pb2.ValuedResponse(type="STOP_AND_RETURN", message=nmsg)
        return reply

    def OnMessageDelivered(self, request, context):
        return exhook_pb2.EmptySuccess()

    def OnMessageDropped(self, request, context):
        return exhook_pb2.EmptySuccess()

    def OnMessageAcked(self, request, context):
        return exhook_pb2.EmptySuccess()

def serve():
    server = grpc.server(futures.ThreadPoolExecutor(max_workers=10))
    exhook_pb2_grpc.add_HookProviderServicer_to_server(HookProvider(), server)
    server.add_insecure_port('[::]:9000')
    server.start()

    print("Started gRPC server on [::]:9000")

    server.wait_for_termination()


if __name__ == '__main__':
    logging.basicConfig()
    serve()
