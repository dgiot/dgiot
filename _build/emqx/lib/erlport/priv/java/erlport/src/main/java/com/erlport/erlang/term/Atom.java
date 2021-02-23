package com.erlport.erlang.term;

public class Atom {

    public String coder = "UTF-8";

    public String value;

    public Atom(String v, String c) {
        this.value = v;
        this.coder = c;
    }

    public Atom(String v) {
        this.value = v;
    }

    @Override
    public String toString() {
        return "Atom{" +
                "value='" + value + '\'' +
                '}';
    }
}
