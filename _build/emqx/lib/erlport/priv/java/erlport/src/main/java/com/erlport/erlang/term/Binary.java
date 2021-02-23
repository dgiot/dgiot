package com.erlport.erlang.term;

import java.util.Arrays;

public class Binary {

    public byte[] raw;

    public Binary(byte[] bytes) {
        this.raw = bytes;
    }

    public Binary(String str) {
        this.raw = str.getBytes();
    }

    @Override
    public String toString() {
        return "Binary{" +
                "raw=" + Arrays.toString(raw) +
                '}';
    }
}
