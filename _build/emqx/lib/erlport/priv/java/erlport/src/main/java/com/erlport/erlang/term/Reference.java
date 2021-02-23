package com.erlport.erlang.term;

public class Reference {
    private Atom node;
    public Long id;
    private Long creation;

    public Reference(Atom node, Long id, Long creation) {
        this.node = node;
        this.id = id;
        this.creation = creation;
    }

    @Override
    public String toString() {
        return "Reference{" +
                "node=" + node +
                ", id=" + id +
                ", creation=" + creation +
                '}';
    }
}
