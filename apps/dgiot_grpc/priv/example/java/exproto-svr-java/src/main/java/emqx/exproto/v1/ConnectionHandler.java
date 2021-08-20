package emqx.exproto.v1;

import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;
import io.grpc.stub.StreamObserver;

import java.util.List;

/**
 * @author WangScaler
 * @date 2021/7/15 18:20
 */

public class ConnectionHandler extends ConnectionHandlerGrpc.ConnectionHandlerImplBase {
    private static final String HOST = "192.168.1.19:9100";
    static ManagedChannel channel;

    static {
        System.out.println("[LOG] Build singleton channel");
        channel = ManagedChannelBuilder
                .forTarget(HOST)
                .usePlaintext()
                .build();
    }

    @Override
    public StreamObserver<Exproto.SocketCreatedRequest> onSocketCreated(final StreamObserver<Exproto.EmptySuccess> responseObserver) {
        return new StreamObserver<Exproto.SocketCreatedRequest>() {
            public void onNext(Exproto.SocketCreatedRequest request) {
                ConnectionAdapterGrpc.ConnectionAdapterBlockingStub blockingStub = ConnectionAdapterGrpc.newBlockingStub(channel);
                System.out.println("[LOG] client socket connection:" + request.getConninfo());
                Exproto.ClientInfo clientInfo = Exproto.ClientInfo.newBuilder()
                        .setClientid("test")
                        .setUsername("test")
                        .build();
                Exproto.AuthenticateRequest authenticateRequest = Exproto.AuthenticateRequest.newBuilder()
                        .setClientinfo(clientInfo)
                        .setConn(request.getConn())
                        .setPassword("test")
                        .build();
                Exproto.CodeResponse response = blockingStub.authenticate(authenticateRequest);
                System.out.println("[LOG] authenticate code: " + response.getCodeValue());
                Exproto.TimerRequest timerRequest = Exproto.TimerRequest.newBuilder()
                        .setConn(request.getConn())
                        .setInterval(20)
                        .setType(Exproto.TimerType.KEEPALIVE)
                        .build();
                Exproto.CodeResponse timerResponse = blockingStub.startTimer(timerRequest);
                System.out.println("[LOG] startTimer code: " + timerResponse.getCodeValue());
                Exproto.SubscribeRequest subscribeRequest = Exproto.SubscribeRequest.newBuilder()
                        .setConn(request.getConn())
                        .setTopic("/test")
                        .setQos(0)
                        .build();
                Exproto.CodeResponse subscribeResponse = blockingStub.subscribe(subscribeRequest);
                System.out.println("[LOG] subscribe  code: " + subscribeResponse.getCodeValue());

            }

            public void onError(Throwable throwable) {
                System.out.println(" onSocketCreated error cause" + throwable.getCause());
                System.out.println(" onSocketCreated error message" + throwable.getMessage());
            }

            public void onCompleted() {
                responseObserver.onNext(Exproto.EmptySuccess.getDefaultInstance());
                responseObserver.onCompleted();
            }
        };
    }

    @Override
    public StreamObserver<Exproto.ReceivedBytesRequest> onReceivedBytes(final StreamObserver<Exproto.EmptySuccess> responseObserver) {
        return new StreamObserver<Exproto.ReceivedBytesRequest>() {
            public void onNext(Exproto.ReceivedBytesRequest request) {
                ConnectionAdapterGrpc.ConnectionAdapterBlockingStub blockingStub = ConnectionAdapterGrpc.newBlockingStub(channel);
                System.out.println("[LOG] ReceivedBytesRequest：" + request.getConn());
                Exproto.PublishRequest publishRequest = Exproto.PublishRequest.newBuilder()
                        .setConn(request.getConn())
                        .setTopic("/test1")
                        .setQos(0)
                        .setPayload(request.getBytes()).build();
                Exproto.CodeResponse response = blockingStub.publish(publishRequest);
                System.out.println("[LOG] publish  code: " + response.getCodeValue());
            }

            public void onError(Throwable throwable) {
                System.out.println(" onReceivedBytes error cause" + throwable.getCause());
                System.out.println(" onReceivedBytes error message" + throwable.getMessage());
            }

            public void onCompleted() {
                responseObserver.onNext(Exproto.EmptySuccess.getDefaultInstance());
                responseObserver.onCompleted();
            }
        };
    }

    @Override
    public StreamObserver<Exproto.ReceivedMessagesRequest> onReceivedMessages(final StreamObserver<Exproto.EmptySuccess> responseObserver) {
        return new StreamObserver<Exproto.ReceivedMessagesRequest>() {
            public void onNext(Exproto.ReceivedMessagesRequest receivedMessagesRequest) {
                ConnectionAdapterGrpc.ConnectionAdapterBlockingStub blockingStub = ConnectionAdapterGrpc.newBlockingStub(channel);
                System.out.println("[LOG] onReceivedMessages：" + receivedMessagesRequest.getConn());
                List<Exproto.Message> messagesList = receivedMessagesRequest.getMessagesList();
                for (Exproto.Message message : messagesList) {
                    System.out.println("Message:" + message.getPayload());
                    Exproto.SendBytesRequest sendBytesRequest = Exproto.SendBytesRequest.newBuilder()
                            .setConn(receivedMessagesRequest.getConn())
                            .setBytes(message.getPayload())
                            .build();
                    Exproto.CodeResponse sendBytesResponse = blockingStub.send(sendBytesRequest);
                    System.out.println("[LOG] sendBytes code: " + sendBytesResponse.getCodeValue());

                }

            }

            public void onError(Throwable throwable) {
                System.out.println(" onReceivedMessages error cause" + throwable.getCause());
                System.out.println(" onReceivedMessages error message" + throwable.getMessage());
            }

            public void onCompleted() {
                responseObserver.onNext(Exproto.EmptySuccess.getDefaultInstance());
                responseObserver.onCompleted();
            }
        };
    }

    @Override
    public StreamObserver<Exproto.TimerTimeoutRequest> onTimerTimeout(final StreamObserver<Exproto.EmptySuccess> responseObserver) {
        return new StreamObserver<Exproto.TimerTimeoutRequest>() {
            public void onNext(Exproto.TimerTimeoutRequest timerTimeoutRequest) {
                ConnectionAdapterGrpc.ConnectionAdapterBlockingStub blockingStub = ConnectionAdapterGrpc.newBlockingStub(channel);
                System.out.println("[LOG] onTimerTimeout");
                Exproto.CloseSocketRequest closeSocketRequest = Exproto.CloseSocketRequest.newBuilder()
                        .setConn(timerTimeoutRequest.getConn())
                        .build();
                Exproto.CodeResponse closeResponse = blockingStub.close(closeSocketRequest);
                System.out.println("[LOG] close code: " + closeResponse.getCodeValue());
            }

            public void onError(Throwable throwable) {
                System.out.println(" onTimerTimeout error cause" + throwable.getCause());
                System.out.println(" onTimerTimeout error message" + throwable.getMessage());
            }

            public void onCompleted() {
                responseObserver.onNext(Exproto.EmptySuccess.getDefaultInstance());
                responseObserver.onCompleted();
            }
        };
    }

    @Override
    public StreamObserver<Exproto.SocketClosedRequest> onSocketClosed(final StreamObserver<Exproto.EmptySuccess> responseObserver) {
        return new StreamObserver<Exproto.SocketClosedRequest>() {
            public void onNext(Exproto.SocketClosedRequest socketClosedRequest) {
                System.out.println("[LOG] onSocketClosed:" + socketClosedRequest.toString());
            }

            public void onError(Throwable throwable) {
                System.out.println(" onSocketClosed error cause" + throwable.getCause());
                System.out.println(" onSocketClosed error message" + throwable.getMessage());
            }

            public void onCompleted() {
                responseObserver.onNext(Exproto.EmptySuccess.getDefaultInstance());
                responseObserver.onCompleted();
            }
        };
    }
}
