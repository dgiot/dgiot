package com.erlport.erlang.term;

public class OpaqueObject {

    public Atom lang;

    public Binary value;

    public OpaqueObject(Atom lang, Binary value) {
        this.lang = lang;
        this.value = value;
    }

    @Override
    public String toString() {
        return "OpaqueObject{" +
                "value='" + value + '\'' +
                '}';
    }
}
