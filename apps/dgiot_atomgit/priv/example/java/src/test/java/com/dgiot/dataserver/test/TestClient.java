package com.dgiot.dataserver.test;

import io.grpc.ManagedChannel;
import io.grpc.ManagedChannelBuilder;
import io.grpc.examples.dlink.DlinkGrpc;
import io.grpc.examples.dlink.HelloRequest;
import io.grpc.examples.dlink.HelloReply;
import io.grpc.examples.dlink.HealthCheckRequest;
import io.grpc.examples.dlink.HealthCheckResponse;
import java.util.concurrent.TimeUnit;

import java.util.Iterator;

/**
 * 客户端
 */
public class TestClient {
    private final ManagedChannel channel;
    private final DlinkGrpc.DlinkBlockingStub blockingStub;
    private static final String host="127.0.0.1";
    private static final int ip=50051;
    public TestClient(String host,int port){
        //usePlaintext表示明文传输，否则需要配置ssl
        //channel  表示通信通道
//        channel= ManagedChannelBuilder.forAddress(host, port).usePlaintext(true).build();
        channel = ManagedChannelBuilder.forAddress(host, port).usePlaintext().build();
        //存根
        blockingStub=DlinkGrpc.newBlockingStub(channel);
    }
    public void shutdown() throws InterruptedException {
        channel.shutdown().awaitTermination(5, TimeUnit.SECONDS);
    }

    public void  testSayHello(String name){
        HelloRequest request=HelloRequest.newBuilder().setName(name).build();
        HelloReply response=blockingStub.sayHello(request);
        System.out.println(response.getMessage());
    }

    public void  testCheck(String name){
        HealthCheckRequest request=HealthCheckRequest.newBuilder().build();
        HealthCheckResponse response=blockingStub.check(request);
        System.out.println(response.getStatus().toString());
    }

    public void  testWatch(String name){
        HealthCheckRequest request=HealthCheckRequest.newBuilder().build();
        Iterator<HealthCheckResponse> response =blockingStub.watch(request);
        while (response.hasNext()) {
            System.out.println(response.next());
        }
    }

    public static void main(String[] args) {
        TestClient client=new TestClient(host,ip);
        for (int i=0;i<=5;i++){
            client.testSayHello("<<<<<result>>>>>:"+i);
        }
    }
}
