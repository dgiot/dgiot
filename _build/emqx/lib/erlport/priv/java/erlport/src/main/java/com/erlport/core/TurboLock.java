package com.erlport.core;

import java.util.concurrent.Exchanger;

/**
 * @author wangwenhai
 * @date 2020/8/12
 * File description: A thread lock
 */
public class TurboLock {
    private boolean ready;
    private Exchanger<Object> exchanger;


    TurboLock(boolean ready, Exchanger<Object> exchanger) {
        this.ready = ready;
        this.exchanger = exchanger;
    }

    public Exchanger<Object> getExchanger() {
        return exchanger;
    }

    public void setExchanger(Exchanger<Object> exchanger) {
        this.exchanger = exchanger;
    }

    boolean isReady() {
        return ready;
    }

    void setReady(boolean ready) {
        this.ready = ready;
    }


    @Override
    public String toString() {
        return "TurboLock{" +
                "ready=" + ready +
                '}';
    }
}
