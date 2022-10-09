package com.dgiot.dataserver.grpcserver;

import io.grpc.Server;
import io.grpc.ServerBuilder;
import io.grpc.examples.dlink.DlinkGrpc;
import io.grpc.examples.dlink.HelloRequest;
import io.grpc.examples.dlink.HelloReply;
import io.grpc.examples.dlink.HealthCheckRequest;
import io.grpc.examples.dlink.HealthCheckResponse;
import io.grpc.stub.StreamObserver;

import java.io.IOException;

/**
 * 服务端
 */

public class TestServer {
    //定义端口
    private final int port = 50051;
    //服务
    private Server server;

    //启动服务,并且接受请求
    private void start() throws IOException {
        server = ServerBuilder.forPort(port).addService(new DlinkImpl()).build().start();
        System.out.println("服务开始启动-------");
        Runtime.getRuntime().addShutdownHook(new Thread() {
            @Override
            public void run() {
                System.err.println("------shutting down gRPC server since JVM is shutting down-------");
                TestServer.this.stop();
                System.err.println("------server shut down------");
            }
        });

    }

    //stop服务
    private void stop() {
        if (server != null) {
            server.shutdown();
        }
    }
    //server阻塞到程序退出
    private void  blockUntilShutdown() throws InterruptedException {
        if (server!=null){
            server.awaitTermination();
        }
    }

    //实现服务接口的类
    private class DlinkImpl extends DlinkGrpc.DlinkImplBase {
        // Sends a greeting
        @Override
        public void sayHello (HelloRequest request, StreamObserver<HelloReply> responseObserver) {
            HelloReply build = HelloReply.newBuilder().setMessage(request.getName()).build();
            //onNext()方法向客户端返回结果
            responseObserver.onNext(build);
            //告诉客户端这次调用已经完成
            responseObserver.onCompleted();
        }


        // If the requested service is unknown, the call will fail with status
        // NOT_FOUND.
        @Override
        public void check(HealthCheckRequest checkRequest, StreamObserver<HealthCheckResponse> responseObserver) {
            System.out.println("健康检查:"+ checkRequest.getService());
            HealthCheckResponse.ServingStatus servingStatus =getServingStatus(checkRequest);
            HealthCheckResponse response = HealthCheckResponse.newBuilder()
                    .setStatus(servingStatus)
                    .build();

            //onNext()方法向客户端返回结果
            responseObserver.onNext(response);
            //告诉客户端这次调用已经完成
            responseObserver.onCompleted();
        }

        // Performs a watch for the serving status of the requested service.
        // The server will immediately send back a message indicating the current
        // serving status.  It will then subsequently send a new message whenever
        // the service's serving status changes.
        //
        // If the requested service is unknown when the call is received, the
        // server will send a message setting the serving status to
        // SERVICE_UNKNOWN but will *not* terminate the call.  If at some
        // future point, the serving status of the service becomes known, the
        // server will send a new message with the service's serving status.
        //
        // If the call terminates with status UNIMPLEMENTED, then clients
        // should assume this method is not supported and should not retry the
        // call.  If the call terminates with any other status (including OK),
        // clients should retry the call with appropriate exponential backoff.
        @Override
        public void watch(HealthCheckRequest checkRequest, StreamObserver<HealthCheckResponse> responseObserver) {
            System.out.println("健康检查 Stream:"+ checkRequest.getService());
            HealthCheckResponse.ServingStatus servingStatus =getServingStatus(checkRequest);
            HealthCheckResponse response = HealthCheckResponse.newBuilder()
                    .setStatus(servingStatus)
                    .build();
            responseObserver.onNext(response);

        }

        private HealthCheckResponse.ServingStatus getServingStatus(HealthCheckRequest request){
            HealthCheckResponse.ServingStatus servingStatus;
            String service = request.getService();
            switch(service){
                case"":
                    servingStatus = HealthCheckResponse.ServingStatus.SERVING;
                    break;
                case"mysql":
                    servingStatus = HealthCheckResponse.ServingStatus.SERVING; //checkMySQL();
                    break;
                case"redis":
                    servingStatus  = HealthCheckResponse.ServingStatus.SERVING;//checkRedis();
                    break;
                default:
                    servingStatus = HealthCheckResponse.ServingStatus.UNKNOWN;
                    break;
            }
            return servingStatus;
        }

    }

    public static void main(String[] args) throws IOException, InterruptedException {

        final  TestServer server=new TestServer();
        server.start();
        server.blockUntilShutdown();

    }
}
