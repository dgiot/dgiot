package emqx.exproto.v1;

import io.grpc.Server;
import io.grpc.netty.shaded.io.grpc.netty.NettyServerBuilder;

import java.io.IOException;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

/**
 * @author WangScaler
 * @date 2021/7/15 18:22
 */

public class TestGrpcServer {
    private static final Logger logger = Logger.getLogger(TestGrpcServer.class.getName());

    public static void main(String[] args) throws IOException, InterruptedException {
        int port = 9001;
        Server server = NettyServerBuilder
                .forPort(9001)
                .permitKeepAliveTime(2, TimeUnit.SECONDS)
                .permitKeepAliveWithoutCalls(true)
                .addService(new ConnectionHandler())
                .build()
                .start();
        logger.info("Server started, listening on " + port);
        server.awaitTermination();
    }
}
