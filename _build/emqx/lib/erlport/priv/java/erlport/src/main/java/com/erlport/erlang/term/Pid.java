package com.erlport.erlang.term;

public class Pid {
    public Atom node;
    public Long id;
    public Long serial;
    public Long creation;

    public Pid(Atom node, Long id, Long serial, Long creation) {
        this.node = node;
        this.id = id;
        this.serial = serial;
        this.creation = creation;
    }


    @Override
    public String toString() {
        return "Pid{" +
                "node=" + node +
                ", id=" + id +
                ", serial=" + serial +
                ", creation=" + creation +
                '}';
    }
}


