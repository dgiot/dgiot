package com.erlport.erlang.term;

public class Port {
    public Atom node;
    public Long id;
    public Long creation;

    public Port(Atom node, Long id, Long creation) {
        this.node = node;
        this.id = id;
        this.creation = creation;
    }


    @Override
    public String toString() {
        return "Port{" +
                "node=" + node +
                ", id=" + id +
                ", creation=" + creation +
                '}';
    }
}


