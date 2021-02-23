package com.erlport.msg;

import com.erlport.erlang.term.Atom;

/**
 * @author wangwenhai
 * @date 2020/7/16
 */
public class ResultMessage extends Message {
    private Object result;

    public ResultMessage() {
        setType(new Atom("'r'"));
    }

    public Object getResult() {
        return result;
    }

    public void setResult(Object result) {
        this.result = result;
    }
}
