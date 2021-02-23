package com.erlport.msg;

import com.erlport.erlang.term.Atom;

import java.util.Arrays;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * @author wangwenhai
 * @date 2020/7/14
 * * Type:
 * * -Call : {'C', Id, Module, Function, Args}
 * * -Message: {'M', Terms}
 */
public class CallMessage extends Message {
    private static AtomicInteger atomicInteger = new AtomicInteger();
    private Atom module;
    private Atom function;
    private Object[] args;

    public CallMessage() {
        this.setType(new Atom("C"));
        if (atomicInteger.incrementAndGet() > 999999998) {
            atomicInteger.set(0);
        }
        setId(atomicInteger.incrementAndGet());
    }

    public Atom getModule() {
        return module;
    }

    public void setModule(Atom module) {
        this.module = module;
    }

    public Atom getFunction() {
        return function;
    }

    public void setFunction(Atom function) {
        this.function = function;
    }

    public Object[] getArgs() {
        return args;
    }

    public void setArgs(Object[] args) {
        this.args = args;
    }

    @Override
    public String toString() {
        return "CallMessage{" +
                "module=" + module +
                ", function=" + function +
                ", args=" + Arrays.toString(args) +
                '}';
    }
}
