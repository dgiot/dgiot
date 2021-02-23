package com.erlport;

import com.erlport.core.JPort;
import com.erlport.erlang.term.Atom;
import com.erlport.msg.CallMessage;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeoutException;

/**
 * @author wangwenhai
 * @date 2020/7/29
 */
public class Erlang {
    /**
     * @param m       : module
     * @param f       : function
     * @param args    : args
     * @param timeout : ms
     * @return Object
     * @throws InterruptedException e
     * @throws ExecutionException   e
     * @throws TimeoutException     e
     */
    public static Object call(String m, String f, Object[] args, long timeout) throws Exception {
        CallMessage callMessage = new CallMessage();
        callMessage.setModule(new Atom(m));
        callMessage.setFunction(new Atom(f));
        callMessage.setArgs(args);
        return JPort.call(callMessage, timeout);
    }
}
