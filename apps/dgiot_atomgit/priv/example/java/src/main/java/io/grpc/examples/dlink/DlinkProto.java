// Generated by the protocol buffer compiler.  DO NOT EDIT!
// source: dlink.proto

package io.grpc.examples.dlink;

public final class DlinkProto {
  private DlinkProto() {}
  public static void registerAllExtensions(
      com.google.protobuf.ExtensionRegistryLite registry) {
  }

  public static void registerAllExtensions(
      com.google.protobuf.ExtensionRegistry registry) {
    registerAllExtensions(
        (com.google.protobuf.ExtensionRegistryLite) registry);
  }
  static final com.google.protobuf.Descriptors.Descriptor
    internal_static_dgiot_HelloRequest_descriptor;
  static final 
    com.google.protobuf.GeneratedMessageV3.FieldAccessorTable
      internal_static_dgiot_HelloRequest_fieldAccessorTable;
  static final com.google.protobuf.Descriptors.Descriptor
    internal_static_dgiot_HelloReply_descriptor;
  static final 
    com.google.protobuf.GeneratedMessageV3.FieldAccessorTable
      internal_static_dgiot_HelloReply_fieldAccessorTable;
  static final com.google.protobuf.Descriptors.Descriptor
    internal_static_dgiot_HealthCheckRequest_descriptor;
  static final 
    com.google.protobuf.GeneratedMessageV3.FieldAccessorTable
      internal_static_dgiot_HealthCheckRequest_fieldAccessorTable;
  static final com.google.protobuf.Descriptors.Descriptor
    internal_static_dgiot_HealthCheckResponse_descriptor;
  static final 
    com.google.protobuf.GeneratedMessageV3.FieldAccessorTable
      internal_static_dgiot_HealthCheckResponse_fieldAccessorTable;

  public static com.google.protobuf.Descriptors.FileDescriptor
      getDescriptor() {
    return descriptor;
  }
  private static  com.google.protobuf.Descriptors.FileDescriptor
      descriptor;
  static {
    java.lang.String[] descriptorData = {
      "\n\013dlink.proto\022\005dgiot\"\034\n\014HelloRequest\022\014\n\004" +
      "name\030\001 \001(\t\"\035\n\nHelloReply\022\017\n\007message\030\001 \001(" +
      "\t\"%\n\022HealthCheckRequest\022\017\n\007service\030\001 \001(\t" +
      "\"\240\001\n\023HealthCheckResponse\0228\n\006status\030\001 \001(\016" +
      "2(.dgiot.HealthCheckResponse.ServingStat" +
      "us\"O\n\rServingStatus\022\013\n\007UNKNOWN\020\000\022\013\n\007SERV" +
      "ING\020\001\022\017\n\013NOT_SERVING\020\002\022\023\n\017SERVICE_UNKNOW" +
      "N\020\0032\277\001\n\005Dlink\0224\n\010SayHello\022\023.dgiot.HelloR" +
      "equest\032\021.dgiot.HelloReply\"\000\022>\n\005Check\022\031.d" +
      "giot.HealthCheckRequest\032\032.dgiot.HealthCh" +
      "eckResponse\022@\n\005Watch\022\031.dgiot.HealthCheck" +
      "Request\032\032.dgiot.HealthCheckResponse0\001B.\n" +
      "\026io.grpc.examples.dlinkB\nDlinkProtoP\001\242\002\005" +
      "dlinkb\006proto3"
    };
    descriptor = com.google.protobuf.Descriptors.FileDescriptor
      .internalBuildGeneratedFileFrom(descriptorData,
        new com.google.protobuf.Descriptors.FileDescriptor[] {
        });
    internal_static_dgiot_HelloRequest_descriptor =
      getDescriptor().getMessageTypes().get(0);
    internal_static_dgiot_HelloRequest_fieldAccessorTable = new
      com.google.protobuf.GeneratedMessageV3.FieldAccessorTable(
        internal_static_dgiot_HelloRequest_descriptor,
        new java.lang.String[] { "Name", });
    internal_static_dgiot_HelloReply_descriptor =
      getDescriptor().getMessageTypes().get(1);
    internal_static_dgiot_HelloReply_fieldAccessorTable = new
      com.google.protobuf.GeneratedMessageV3.FieldAccessorTable(
        internal_static_dgiot_HelloReply_descriptor,
        new java.lang.String[] { "Message", });
    internal_static_dgiot_HealthCheckRequest_descriptor =
      getDescriptor().getMessageTypes().get(2);
    internal_static_dgiot_HealthCheckRequest_fieldAccessorTable = new
      com.google.protobuf.GeneratedMessageV3.FieldAccessorTable(
        internal_static_dgiot_HealthCheckRequest_descriptor,
        new java.lang.String[] { "Service", });
    internal_static_dgiot_HealthCheckResponse_descriptor =
      getDescriptor().getMessageTypes().get(3);
    internal_static_dgiot_HealthCheckResponse_fieldAccessorTable = new
      com.google.protobuf.GeneratedMessageV3.FieldAccessorTable(
        internal_static_dgiot_HealthCheckResponse_descriptor,
        new java.lang.String[] { "Status", });
  }

  // @@protoc_insertion_point(outer_class_scope)
}
