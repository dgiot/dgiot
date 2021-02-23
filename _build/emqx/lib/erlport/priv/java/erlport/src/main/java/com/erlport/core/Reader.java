package com.erlport.core;

import com.erlport.erlang.term.Atom;
import com.erlport.erlang.term.Binary;
import com.erlport.erlang.term.Tuple;
import com.erlport.proto.*;

import java.io.EOFException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.concurrent.ConcurrentHashMap;

/**
 * @author wangwenhai
 * @date 2020/7/11
 */
public class Reader extends Thread {
    private ConcurrentHashMap<Class<?>, Object> classCache = new ConcurrentHashMap<>();
    private Channel channel;

    Reader(Channel channel) {
        setName("IOReadThread");
        this.channel = channel;
    }

    @Override
    public void run() {
        for (; ; ) {
            try {
                Request request = channel.read();
                try {
                    if (request == null) {
                        continue;
                    }
                    if (request.type == RequestType.CALL) {
                        JPort.executorService.submit(() -> {
                            try {
                                Class<?> clazz = Class.forName(request.classname.value);
                                Object instance = classCache.get(clazz);
                                if (instance == null) {
                                    instance = clazz.newInstance();
                                    classCache.put(clazz, instance);
                                }
                                Class<?>[] classArgs = new Class[request.args.length];
                                Arrays.fill(classArgs, Object.class);
                                Method method = clazz.getMethod(request.methodName.value, classArgs);

                                Object result = method.invoke(instance, request.args);

                                if (result == null) {
                                    result = new Atom("ok");
                                }
                                channel.write(Response.success(request.requestId, result));
                            } catch (Exception e) {
                                e.printStackTrace();
                            }
                        });
                    } else if (request.type == RequestType.RESULT ||
                               request.type == RequestType.ERROR) {
                        // [type, Id, Result]
                        // [Atom("r"), 2, Tuple{elements=[Atom("resp"), Atom("x")]}]

                      //  JPort.executorService.submit(() -> {
                            Tuple tuple = (Tuple) request.rawTerm;
                            if (tuple.length() == 3) {
                                Integer id = (Integer) tuple.get(1);
                                if (JPort.REQUEST_MAP.get(id) != null) {
                                    try {
                                        JPort.REQUEST_MAP.get(id).getExchanger().exchange(tuple.get(2));
                                    } catch (InterruptedException e) {
                                        //e.printStackTrace();
                                    }
                                }
                            }
                       // });
                    }
                } catch (Exception e) {
                    // e.printStackTrace();
                    Binary errDesc = Utils.stringToBinary(Utils.getStackTrace(e));
                    channel.write(Response.failure(request.requestId, errDesc));
                }
            } catch (Exception e1) {
                // e1.printStackTrace();

                if (e1 instanceof EOFException) {
                    System.err.println("[LOG] System exited with message:" + e1.getMessage());
                    System.exit(0);
                }
                Binary errDesc = Utils.stringToBinary(Utils.getStackTrace(e1));
                try {
                    channel.write(Response.stop(errDesc));
                } catch (Exception e2) {
                    // e2.printStackTrace();
                }


            }
        }

    }
}
