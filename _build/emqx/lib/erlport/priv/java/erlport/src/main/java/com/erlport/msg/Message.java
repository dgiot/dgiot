package com.erlport.msg;

import com.erlport.erlang.term.Atom;

/**
 * @author wangwenhai
 * @date 2020/7/14
 * File description: Exchange message
 */
public class Message {
    private Atom type;
    private Integer id;

    public Atom getType() {
        return type;
    }

    public void setType(Atom type) {
        this.type = type;
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }
}
