package com.erlport.core;

import com.erlport.msg.CallMessage;
import com.erlport.proto.Channel;
import com.erlport.proto.Options;

import java.util.concurrent.*;

/**
 * @author wangwenhai
 * @date 2020/7/15
 */
public class JPort {
    static final ExecutorService executorService = Executors.newCachedThreadPool();
    // K: MessageId V: UUID Lock
    static final ConcurrentHashMap<Integer, TurboLock> REQUEST_MAP = new ConcurrentHashMap<>();
    // K: MessageId V: Call Result
    static final ConcurrentHashMap<Integer, Object> RESULT_MAP = new ConcurrentHashMap<>();
    // Read Channel
    private static Channel channel;

    /**
     * @param args Cli args
     */
    public static void start(String[] args) {
        Options options = new Options(args);
        channel = new Channel(options);
        Reader reader = new Reader(channel);
        executorService.submit(reader);
    }

    /**
     * synchronized call
     *
     * @param message Call msg
     * @param timeout Timeout
     * @return ErlangTerm Object
     */
    public static Object call(CallMessage message, long timeout) throws Exception {

        final Exchanger<Object> exchanger = new Exchanger<>();
        final TurboLock turboLock = new TurboLock(false, exchanger);
        JPort.REQUEST_MAP.put(message.getId(), turboLock);
        channel.write(message);
        Object result = exchanger.exchange(message.getId(), timeout, TimeUnit.MILLISECONDS);
        JPort.REQUEST_MAP.remove(message.getId());
        return result;
    }
}
