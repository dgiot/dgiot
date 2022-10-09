package io.grpc.examples.dlink;

import static io.grpc.MethodDescriptor.generateFullMethodName;
import static io.grpc.stub.ClientCalls.asyncBidiStreamingCall;
import static io.grpc.stub.ClientCalls.asyncClientStreamingCall;
import static io.grpc.stub.ClientCalls.asyncServerStreamingCall;
import static io.grpc.stub.ClientCalls.asyncUnaryCall;
import static io.grpc.stub.ClientCalls.blockingServerStreamingCall;
import static io.grpc.stub.ClientCalls.blockingUnaryCall;
import static io.grpc.stub.ClientCalls.futureUnaryCall;
import static io.grpc.stub.ServerCalls.asyncBidiStreamingCall;
import static io.grpc.stub.ServerCalls.asyncClientStreamingCall;
import static io.grpc.stub.ServerCalls.asyncServerStreamingCall;
import static io.grpc.stub.ServerCalls.asyncUnaryCall;
import static io.grpc.stub.ServerCalls.asyncUnimplementedStreamingCall;
import static io.grpc.stub.ServerCalls.asyncUnimplementedUnaryCall;

/**
 * <pre>
 * The dlink service definition.
 * </pre>
 */
@javax.annotation.Generated(
    value = "by gRPC proto compiler (version 1.26.0)",
    comments = "Source: dlink.proto")
public final class DlinkGrpc {

  private DlinkGrpc() {}

  public static final String SERVICE_NAME = "dgiot.Dlink";

  // Static method descriptors that strictly reflect the proto.
  private static volatile io.grpc.MethodDescriptor<io.grpc.examples.dlink.HelloRequest,
      io.grpc.examples.dlink.HelloReply> getSayHelloMethod;

  @io.grpc.stub.annotations.RpcMethod(
      fullMethodName = SERVICE_NAME + '/' + "SayHello",
      requestType = io.grpc.examples.dlink.HelloRequest.class,
      responseType = io.grpc.examples.dlink.HelloReply.class,
      methodType = io.grpc.MethodDescriptor.MethodType.UNARY)
  public static io.grpc.MethodDescriptor<io.grpc.examples.dlink.HelloRequest,
      io.grpc.examples.dlink.HelloReply> getSayHelloMethod() {
    io.grpc.MethodDescriptor<io.grpc.examples.dlink.HelloRequest, io.grpc.examples.dlink.HelloReply> getSayHelloMethod;
    if ((getSayHelloMethod = DlinkGrpc.getSayHelloMethod) == null) {
      synchronized (DlinkGrpc.class) {
        if ((getSayHelloMethod = DlinkGrpc.getSayHelloMethod) == null) {
          DlinkGrpc.getSayHelloMethod = getSayHelloMethod =
              io.grpc.MethodDescriptor.<io.grpc.examples.dlink.HelloRequest, io.grpc.examples.dlink.HelloReply>newBuilder()
              .setType(io.grpc.MethodDescriptor.MethodType.UNARY)
              .setFullMethodName(generateFullMethodName(SERVICE_NAME, "SayHello"))
              .setSampledToLocalTracing(true)
              .setRequestMarshaller(io.grpc.protobuf.ProtoUtils.marshaller(
                  io.grpc.examples.dlink.HelloRequest.getDefaultInstance()))
              .setResponseMarshaller(io.grpc.protobuf.ProtoUtils.marshaller(
                  io.grpc.examples.dlink.HelloReply.getDefaultInstance()))
              .setSchemaDescriptor(new DlinkMethodDescriptorSupplier("SayHello"))
              .build();
        }
      }
    }
    return getSayHelloMethod;
  }

  private static volatile io.grpc.MethodDescriptor<io.grpc.examples.dlink.HealthCheckRequest,
      io.grpc.examples.dlink.HealthCheckResponse> getCheckMethod;

  @io.grpc.stub.annotations.RpcMethod(
      fullMethodName = SERVICE_NAME + '/' + "Check",
      requestType = io.grpc.examples.dlink.HealthCheckRequest.class,
      responseType = io.grpc.examples.dlink.HealthCheckResponse.class,
      methodType = io.grpc.MethodDescriptor.MethodType.UNARY)
  public static io.grpc.MethodDescriptor<io.grpc.examples.dlink.HealthCheckRequest,
      io.grpc.examples.dlink.HealthCheckResponse> getCheckMethod() {
    io.grpc.MethodDescriptor<io.grpc.examples.dlink.HealthCheckRequest, io.grpc.examples.dlink.HealthCheckResponse> getCheckMethod;
    if ((getCheckMethod = DlinkGrpc.getCheckMethod) == null) {
      synchronized (DlinkGrpc.class) {
        if ((getCheckMethod = DlinkGrpc.getCheckMethod) == null) {
          DlinkGrpc.getCheckMethod = getCheckMethod =
              io.grpc.MethodDescriptor.<io.grpc.examples.dlink.HealthCheckRequest, io.grpc.examples.dlink.HealthCheckResponse>newBuilder()
              .setType(io.grpc.MethodDescriptor.MethodType.UNARY)
              .setFullMethodName(generateFullMethodName(SERVICE_NAME, "Check"))
              .setSampledToLocalTracing(true)
              .setRequestMarshaller(io.grpc.protobuf.ProtoUtils.marshaller(
                  io.grpc.examples.dlink.HealthCheckRequest.getDefaultInstance()))
              .setResponseMarshaller(io.grpc.protobuf.ProtoUtils.marshaller(
                  io.grpc.examples.dlink.HealthCheckResponse.getDefaultInstance()))
              .setSchemaDescriptor(new DlinkMethodDescriptorSupplier("Check"))
              .build();
        }
      }
    }
    return getCheckMethod;
  }

  private static volatile io.grpc.MethodDescriptor<io.grpc.examples.dlink.HealthCheckRequest,
      io.grpc.examples.dlink.HealthCheckResponse> getWatchMethod;

  @io.grpc.stub.annotations.RpcMethod(
      fullMethodName = SERVICE_NAME + '/' + "Watch",
      requestType = io.grpc.examples.dlink.HealthCheckRequest.class,
      responseType = io.grpc.examples.dlink.HealthCheckResponse.class,
      methodType = io.grpc.MethodDescriptor.MethodType.SERVER_STREAMING)
  public static io.grpc.MethodDescriptor<io.grpc.examples.dlink.HealthCheckRequest,
      io.grpc.examples.dlink.HealthCheckResponse> getWatchMethod() {
    io.grpc.MethodDescriptor<io.grpc.examples.dlink.HealthCheckRequest, io.grpc.examples.dlink.HealthCheckResponse> getWatchMethod;
    if ((getWatchMethod = DlinkGrpc.getWatchMethod) == null) {
      synchronized (DlinkGrpc.class) {
        if ((getWatchMethod = DlinkGrpc.getWatchMethod) == null) {
          DlinkGrpc.getWatchMethod = getWatchMethod =
              io.grpc.MethodDescriptor.<io.grpc.examples.dlink.HealthCheckRequest, io.grpc.examples.dlink.HealthCheckResponse>newBuilder()
              .setType(io.grpc.MethodDescriptor.MethodType.SERVER_STREAMING)
              .setFullMethodName(generateFullMethodName(SERVICE_NAME, "Watch"))
              .setSampledToLocalTracing(true)
              .setRequestMarshaller(io.grpc.protobuf.ProtoUtils.marshaller(
                  io.grpc.examples.dlink.HealthCheckRequest.getDefaultInstance()))
              .setResponseMarshaller(io.grpc.protobuf.ProtoUtils.marshaller(
                  io.grpc.examples.dlink.HealthCheckResponse.getDefaultInstance()))
              .setSchemaDescriptor(new DlinkMethodDescriptorSupplier("Watch"))
              .build();
        }
      }
    }
    return getWatchMethod;
  }

  /**
   * Creates a new async stub that supports all call types for the service
   */
  public static DlinkStub newStub(io.grpc.Channel channel) {
    io.grpc.stub.AbstractStub.StubFactory<DlinkStub> factory =
      new io.grpc.stub.AbstractStub.StubFactory<DlinkStub>() {
        @java.lang.Override
        public DlinkStub newStub(io.grpc.Channel channel, io.grpc.CallOptions callOptions) {
          return new DlinkStub(channel, callOptions);
        }
      };
    return DlinkStub.newStub(factory, channel);
  }

  /**
   * Creates a new blocking-style stub that supports unary and streaming output calls on the service
   */
  public static DlinkBlockingStub newBlockingStub(
      io.grpc.Channel channel) {
    io.grpc.stub.AbstractStub.StubFactory<DlinkBlockingStub> factory =
      new io.grpc.stub.AbstractStub.StubFactory<DlinkBlockingStub>() {
        @java.lang.Override
        public DlinkBlockingStub newStub(io.grpc.Channel channel, io.grpc.CallOptions callOptions) {
          return new DlinkBlockingStub(channel, callOptions);
        }
      };
    return DlinkBlockingStub.newStub(factory, channel);
  }

  /**
   * Creates a new ListenableFuture-style stub that supports unary calls on the service
   */
  public static DlinkFutureStub newFutureStub(
      io.grpc.Channel channel) {
    io.grpc.stub.AbstractStub.StubFactory<DlinkFutureStub> factory =
      new io.grpc.stub.AbstractStub.StubFactory<DlinkFutureStub>() {
        @java.lang.Override
        public DlinkFutureStub newStub(io.grpc.Channel channel, io.grpc.CallOptions callOptions) {
          return new DlinkFutureStub(channel, callOptions);
        }
      };
    return DlinkFutureStub.newStub(factory, channel);
  }

  /**
   * <pre>
   * The dlink service definition.
   * </pre>
   */
  public static abstract class DlinkImplBase implements io.grpc.BindableService {

    /**
     * <pre>
     * Sends a greeting
     * </pre>
     */
    public void sayHello(io.grpc.examples.dlink.HelloRequest request,
        io.grpc.stub.StreamObserver<io.grpc.examples.dlink.HelloReply> responseObserver) {
      asyncUnimplementedUnaryCall(getSayHelloMethod(), responseObserver);
    }

    /**
     * <pre>
     * If the requested service is unknown, the call will fail with status
     * NOT_FOUND.
     * </pre>
     */
    public void check(io.grpc.examples.dlink.HealthCheckRequest request,
        io.grpc.stub.StreamObserver<io.grpc.examples.dlink.HealthCheckResponse> responseObserver) {
      asyncUnimplementedUnaryCall(getCheckMethod(), responseObserver);
    }

    /**
     * <pre>
     * Performs a watch for the serving status of the requested service.
     * The server will immediately send back a message indicating the current
     * serving status.  It will then subsequently send a new message whenever
     * the service's serving status changes.
     * If the requested service is unknown when the call is received, the
     * server will send a message setting the serving status to
     * SERVICE_UNKNOWN but will *not* terminate the call.  If at some
     * future point, the serving status of the service becomes known, the
     * server will send a new message with the service's serving status.
     * If the call terminates with status UNIMPLEMENTED, then clients
     * should assume this method is not supported and should not retry the
     * call.  If the call terminates with any other status (including OK),
     * clients should retry the call with appropriate exponential backoff.
     * </pre>
     */
    public void watch(io.grpc.examples.dlink.HealthCheckRequest request,
        io.grpc.stub.StreamObserver<io.grpc.examples.dlink.HealthCheckResponse> responseObserver) {
      asyncUnimplementedUnaryCall(getWatchMethod(), responseObserver);
    }

    @java.lang.Override public final io.grpc.ServerServiceDefinition bindService() {
      return io.grpc.ServerServiceDefinition.builder(getServiceDescriptor())
          .addMethod(
            getSayHelloMethod(),
            asyncUnaryCall(
              new MethodHandlers<
                io.grpc.examples.dlink.HelloRequest,
                io.grpc.examples.dlink.HelloReply>(
                  this, METHODID_SAY_HELLO)))
          .addMethod(
            getCheckMethod(),
            asyncUnaryCall(
              new MethodHandlers<
                io.grpc.examples.dlink.HealthCheckRequest,
                io.grpc.examples.dlink.HealthCheckResponse>(
                  this, METHODID_CHECK)))
          .addMethod(
            getWatchMethod(),
            asyncServerStreamingCall(
              new MethodHandlers<
                io.grpc.examples.dlink.HealthCheckRequest,
                io.grpc.examples.dlink.HealthCheckResponse>(
                  this, METHODID_WATCH)))
          .build();
    }
  }

  /**
   * <pre>
   * The dlink service definition.
   * </pre>
   */
  public static final class DlinkStub extends io.grpc.stub.AbstractAsyncStub<DlinkStub> {
    private DlinkStub(
        io.grpc.Channel channel, io.grpc.CallOptions callOptions) {
      super(channel, callOptions);
    }

    @java.lang.Override
    protected DlinkStub build(
        io.grpc.Channel channel, io.grpc.CallOptions callOptions) {
      return new DlinkStub(channel, callOptions);
    }

    /**
     * <pre>
     * Sends a greeting
     * </pre>
     */
    public void sayHello(io.grpc.examples.dlink.HelloRequest request,
        io.grpc.stub.StreamObserver<io.grpc.examples.dlink.HelloReply> responseObserver) {
      asyncUnaryCall(
          getChannel().newCall(getSayHelloMethod(), getCallOptions()), request, responseObserver);
    }

    /**
     * <pre>
     * If the requested service is unknown, the call will fail with status
     * NOT_FOUND.
     * </pre>
     */
    public void check(io.grpc.examples.dlink.HealthCheckRequest request,
        io.grpc.stub.StreamObserver<io.grpc.examples.dlink.HealthCheckResponse> responseObserver) {
      asyncUnaryCall(
          getChannel().newCall(getCheckMethod(), getCallOptions()), request, responseObserver);
    }

    /**
     * <pre>
     * Performs a watch for the serving status of the requested service.
     * The server will immediately send back a message indicating the current
     * serving status.  It will then subsequently send a new message whenever
     * the service's serving status changes.
     * If the requested service is unknown when the call is received, the
     * server will send a message setting the serving status to
     * SERVICE_UNKNOWN but will *not* terminate the call.  If at some
     * future point, the serving status of the service becomes known, the
     * server will send a new message with the service's serving status.
     * If the call terminates with status UNIMPLEMENTED, then clients
     * should assume this method is not supported and should not retry the
     * call.  If the call terminates with any other status (including OK),
     * clients should retry the call with appropriate exponential backoff.
     * </pre>
     */
    public void watch(io.grpc.examples.dlink.HealthCheckRequest request,
        io.grpc.stub.StreamObserver<io.grpc.examples.dlink.HealthCheckResponse> responseObserver) {
      asyncServerStreamingCall(
          getChannel().newCall(getWatchMethod(), getCallOptions()), request, responseObserver);
    }
  }

  /**
   * <pre>
   * The dlink service definition.
   * </pre>
   */
  public static final class DlinkBlockingStub extends io.grpc.stub.AbstractBlockingStub<DlinkBlockingStub> {
    private DlinkBlockingStub(
        io.grpc.Channel channel, io.grpc.CallOptions callOptions) {
      super(channel, callOptions);
    }

    @java.lang.Override
    protected DlinkBlockingStub build(
        io.grpc.Channel channel, io.grpc.CallOptions callOptions) {
      return new DlinkBlockingStub(channel, callOptions);
    }

    /**
     * <pre>
     * Sends a greeting
     * </pre>
     */
    public io.grpc.examples.dlink.HelloReply sayHello(io.grpc.examples.dlink.HelloRequest request) {
      return blockingUnaryCall(
          getChannel(), getSayHelloMethod(), getCallOptions(), request);
    }

    /**
     * <pre>
     * If the requested service is unknown, the call will fail with status
     * NOT_FOUND.
     * </pre>
     */
    public io.grpc.examples.dlink.HealthCheckResponse check(io.grpc.examples.dlink.HealthCheckRequest request) {
      return blockingUnaryCall(
          getChannel(), getCheckMethod(), getCallOptions(), request);
    }

    /**
     * <pre>
     * Performs a watch for the serving status of the requested service.
     * The server will immediately send back a message indicating the current
     * serving status.  It will then subsequently send a new message whenever
     * the service's serving status changes.
     * If the requested service is unknown when the call is received, the
     * server will send a message setting the serving status to
     * SERVICE_UNKNOWN but will *not* terminate the call.  If at some
     * future point, the serving status of the service becomes known, the
     * server will send a new message with the service's serving status.
     * If the call terminates with status UNIMPLEMENTED, then clients
     * should assume this method is not supported and should not retry the
     * call.  If the call terminates with any other status (including OK),
     * clients should retry the call with appropriate exponential backoff.
     * </pre>
     */
    public java.util.Iterator<io.grpc.examples.dlink.HealthCheckResponse> watch(
        io.grpc.examples.dlink.HealthCheckRequest request) {
      return blockingServerStreamingCall(
          getChannel(), getWatchMethod(), getCallOptions(), request);
    }
  }

  /**
   * <pre>
   * The dlink service definition.
   * </pre>
   */
  public static final class DlinkFutureStub extends io.grpc.stub.AbstractFutureStub<DlinkFutureStub> {
    private DlinkFutureStub(
        io.grpc.Channel channel, io.grpc.CallOptions callOptions) {
      super(channel, callOptions);
    }

    @java.lang.Override
    protected DlinkFutureStub build(
        io.grpc.Channel channel, io.grpc.CallOptions callOptions) {
      return new DlinkFutureStub(channel, callOptions);
    }

    /**
     * <pre>
     * Sends a greeting
     * </pre>
     */
    public com.google.common.util.concurrent.ListenableFuture<io.grpc.examples.dlink.HelloReply> sayHello(
        io.grpc.examples.dlink.HelloRequest request) {
      return futureUnaryCall(
          getChannel().newCall(getSayHelloMethod(), getCallOptions()), request);
    }

    /**
     * <pre>
     * If the requested service is unknown, the call will fail with status
     * NOT_FOUND.
     * </pre>
     */
    public com.google.common.util.concurrent.ListenableFuture<io.grpc.examples.dlink.HealthCheckResponse> check(
        io.grpc.examples.dlink.HealthCheckRequest request) {
      return futureUnaryCall(
          getChannel().newCall(getCheckMethod(), getCallOptions()), request);
    }
  }

  private static final int METHODID_SAY_HELLO = 0;
  private static final int METHODID_CHECK = 1;
  private static final int METHODID_WATCH = 2;

  private static final class MethodHandlers<Req, Resp> implements
      io.grpc.stub.ServerCalls.UnaryMethod<Req, Resp>,
      io.grpc.stub.ServerCalls.ServerStreamingMethod<Req, Resp>,
      io.grpc.stub.ServerCalls.ClientStreamingMethod<Req, Resp>,
      io.grpc.stub.ServerCalls.BidiStreamingMethod<Req, Resp> {
    private final DlinkImplBase serviceImpl;
    private final int methodId;

    MethodHandlers(DlinkImplBase serviceImpl, int methodId) {
      this.serviceImpl = serviceImpl;
      this.methodId = methodId;
    }

    @java.lang.Override
    @java.lang.SuppressWarnings("unchecked")
    public void invoke(Req request, io.grpc.stub.StreamObserver<Resp> responseObserver) {
      switch (methodId) {
        case METHODID_SAY_HELLO:
          serviceImpl.sayHello((io.grpc.examples.dlink.HelloRequest) request,
              (io.grpc.stub.StreamObserver<io.grpc.examples.dlink.HelloReply>) responseObserver);
          break;
        case METHODID_CHECK:
          serviceImpl.check((io.grpc.examples.dlink.HealthCheckRequest) request,
              (io.grpc.stub.StreamObserver<io.grpc.examples.dlink.HealthCheckResponse>) responseObserver);
          break;
        case METHODID_WATCH:
          serviceImpl.watch((io.grpc.examples.dlink.HealthCheckRequest) request,
              (io.grpc.stub.StreamObserver<io.grpc.examples.dlink.HealthCheckResponse>) responseObserver);
          break;
        default:
          throw new AssertionError();
      }
    }

    @java.lang.Override
    @java.lang.SuppressWarnings("unchecked")
    public io.grpc.stub.StreamObserver<Req> invoke(
        io.grpc.stub.StreamObserver<Resp> responseObserver) {
      switch (methodId) {
        default:
          throw new AssertionError();
      }
    }
  }

  private static abstract class DlinkBaseDescriptorSupplier
      implements io.grpc.protobuf.ProtoFileDescriptorSupplier, io.grpc.protobuf.ProtoServiceDescriptorSupplier {
    DlinkBaseDescriptorSupplier() {}

    @java.lang.Override
    public com.google.protobuf.Descriptors.FileDescriptor getFileDescriptor() {
      return io.grpc.examples.dlink.DlinkProto.getDescriptor();
    }

    @java.lang.Override
    public com.google.protobuf.Descriptors.ServiceDescriptor getServiceDescriptor() {
      return getFileDescriptor().findServiceByName("Dlink");
    }
  }

  private static final class DlinkFileDescriptorSupplier
      extends DlinkBaseDescriptorSupplier {
    DlinkFileDescriptorSupplier() {}
  }

  private static final class DlinkMethodDescriptorSupplier
      extends DlinkBaseDescriptorSupplier
      implements io.grpc.protobuf.ProtoMethodDescriptorSupplier {
    private final String methodName;

    DlinkMethodDescriptorSupplier(String methodName) {
      this.methodName = methodName;
    }

    @java.lang.Override
    public com.google.protobuf.Descriptors.MethodDescriptor getMethodDescriptor() {
      return getServiceDescriptor().findMethodByName(methodName);
    }
  }

  private static volatile io.grpc.ServiceDescriptor serviceDescriptor;

  public static io.grpc.ServiceDescriptor getServiceDescriptor() {
    io.grpc.ServiceDescriptor result = serviceDescriptor;
    if (result == null) {
      synchronized (DlinkGrpc.class) {
        result = serviceDescriptor;
        if (result == null) {
          serviceDescriptor = result = io.grpc.ServiceDescriptor.newBuilder(SERVICE_NAME)
              .setSchemaDescriptor(new DlinkFileDescriptorSupplier())
              .addMethod(getSayHelloMethod())
              .addMethod(getCheckMethod())
              .addMethod(getWatchMethod())
              .build();
        }
      }
    }
    return result;
  }
}
