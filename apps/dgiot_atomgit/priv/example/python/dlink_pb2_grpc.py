# Generated by the gRPC Python protocol compiler plugin. DO NOT EDIT!
"""Client and server classes corresponding to protobuf-defined services."""
import grpc

import dlink_pb2 as dlink__pb2


class DlinkStub(object):
    """The dlink service definition.
    """

    def __init__(self, channel):
        """Constructor.

        Args:
            channel: A grpc.Channel.
        """
        self.Payload = channel.unary_unary(
                '/dgiot.Dlink/Payload',
                request_serializer=dlink__pb2.PayloadRequest.SerializeToString,
                response_deserializer=dlink__pb2.PayloadResponse.FromString,
                )


class DlinkServicer(object):
    """The dlink service definition.
    """

    def Payload(self, request, context):
        """Missing associated documentation comment in .proto file."""
        context.set_code(grpc.StatusCode.UNIMPLEMENTED)
        context.set_details('Method not implemented!')
        raise NotImplementedError('Method not implemented!')


def add_DlinkServicer_to_server(servicer, server):
    rpc_method_handlers = {
            'Payload': grpc.unary_unary_rpc_method_handler(
                    servicer.Payload,
                    request_deserializer=dlink__pb2.PayloadRequest.FromString,
                    response_serializer=dlink__pb2.PayloadResponse.SerializeToString,
            ),
    }
    generic_handler = grpc.method_handlers_generic_handler(
            'dgiot.Dlink', rpc_method_handlers)
    server.add_generic_rpc_handlers((generic_handler,))


 # This class is part of an EXPERIMENTAL API.
class Dlink(object):
    """The dlink service definition.
    """

    @staticmethod
    def Payload(request,
            target,
            options=(),
            channel_credentials=None,
            call_credentials=None,
            insecure=False,
            compression=None,
            wait_for_ready=None,
            timeout=None,
            metadata=None):
        return grpc.experimental.unary_unary(request, target, '/dgiot.Dlink/Payload',
            dlink__pb2.PayloadRequest.SerializeToString,
            dlink__pb2.PayloadResponse.FromString,
            options, channel_credentials,
            insecure, call_credentials, compression, wait_for_ready, timeout, metadata)
